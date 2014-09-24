import java.io._
import java.net._
import util.matching.Regex
import collection.mutable.HashMap

import scala.slick.driver.MySQLDriver.simple._
import scala.slick.lifted.ProvenShape
import com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException
import scala.util.control.Exception.ignoring

// http://www.w3.org/TR/REC-xml/#NT-Name
object proforma {

    class Songs(tag: Tag) extends Table[(Option[Int], String, String, String)](tag, "SONGS") {
        def id     = column[Option[Int]]("ID", O.NotNull, O.PrimaryKey, O.AutoInc)
        def url    = column[String]("URL", O.NotNull)
        def artist = column[String]("ARTIST")
        def title  = column[String]("title")

        def * : ProvenShape[(Option[Int], String, String, String)] = (id, url, artist, title)
    }

    val songs = TableQuery[Songs]

    val db = Database.forURL("jdbc:mysql://localhost:3306/savedlinks",
                             driver="com.mysql.jdbc.Driver",
                             user="",
                             password="")

    def main(args: Array[String]) {

        db.withSession{ implicit session =>
            ignoring(classOf[MySQLSyntaxErrorException]) {
                songs.ddl.create
            }
        }


        val port = 18081

        try {
            val ssock = new ServerSocket(port)
            println(s"$port has been opened")

            while(true) {
                val sock   = ssock.accept()
                println("Client has made socket connection")

                val client = new OneConnection(sock)

                client.out.print("HTTP/1.1 200 OK\r\n")

                val reqs = client.getRequests()

                print(reqs)

                val formed = forms(reqs)

                formed.foreach{case(k,v)=>println(s"$k : $v")}

                val l = formed.values.toList.map(mysliClean(_))
                if (l.length>0 && l(0) != "" && l.length == 3) {
                    db.withSession{ implicit session =>
                        val t = (None, "", "", "")
                        songs += t.copy(_2 = l(0), _3 = l(1), _4 = l(2)) // Feels dirty

                        songs.foreach{ case(id, url, artist, title) => println(s" $id\t$url\t$artist\t$title") }
                    }
                    val html = fileString("htmls/index.html")

                    val srvCntLen = html.length()
                    client.out.print(s"Content-Length: $srvCntLen\r\n")
                    client.out.print(s"Content-type: text/html\r\n\r\n$html")
                    client.out.flush()
                }
                else if (l.length == 1 && l(0) == "touchme") {
                    db.withSession{ implicit session =>
                        var html = fileString("htmls/viewstart.html")

                        val allSongs = songs.list

                        // Doing decoding manually'd be trivial were it not for non-ASCII chars.
                        def urlDec(u: String): String = {
                            val re = """\\(?!\\)""".r
                            // Though it doesn't seem to work anyhow...
                            re.replaceAllIn(URLDecoder.decode(u, "UTF-8"), "")
                        }
                        allSongs.foreach { case(_,u,a,t) =>
                            html += s"${urlDec(u)}</br>\n"
                            html += s"${if(a=="")"EMPTY" else urlDec(a)}</br>\n"
                            html += s"${if(t=="")"EMPTY" else urlDec(t)}</br></br>\n\n"
                        }
                        html += "\n</body>\n</html>"

                        client.out.print(s"Content-Length: ${html.length+20}\r\n") // A little extra, just in case
                        client.out.print(s"Content-type: text/html\r\n\r\n$html")
                        client.out.flush()

                        allSongs.foreach{case(_,u,a,t) => print(s"${urlDec(u)} ${urlDec(a)} ${urlDec(t)}\n")}
                    }
                }
                else {
                    val html = fileString("htmls/index.html")

                    val srvCntLen = html.length()
                    client.out.print(s"Content-Length: $srvCntLen\r\n")
                    client.out.print(s"Content-type: text/html\r\n\r\n$html")
                    client.out.flush()
                }

                sock.close()
            }
        }
        catch {
            case e: IOException =>
                System.err.println(s"Could not listen on port $port")
                System.exit(1)
        }

        // The \u0100-\uffff isn't strictly speaking necessary, since
        // the input has at this point already been URL encoded.
        def mysliClean(ill: String): String = {
            var rep = new Regex("([^a-zA-Z0-9\u0100-\uffff])", "sick")
            rep.replaceAllIn(ill, m => "\\\\"+m.group("sick"))
        }

        def fileString(filename: String): String = {
            val src   = io.Source.fromFile(filename)
            val html  = src.getLines.mkString("\n")
            src.close()
            html
        }

        def forms(reqs: String): HashMap[String, String] = {
            val gp    = new Regex("""^GET /.*?\?(.+?=.+?)? HTTP/\d\.\d""")
            val pp    = new Regex("""^POST /.*? HTTP/\d\.\d""")
            val fr    = new Regex("""\r\n\r\n(.+?(=.+?)?)$""")

            var rmap  = HashMap[String, String]()

            def mapify(s: String) {
                s.split("&").foreach{ e =>
                    val ae = e.split("=")
                    rmap(ae(0)) = if(ae.length<2) "" else ae(1)
                }
            }
            try {
                for((l,c)<-reqs.split("\r\n").zipWithIndex) {
                    if (c==0) {
                        l match {
                            case gp(g) => mapify(g)
                            case _ => ""
                        }
                    }
                    if((pp.findFirstIn(l)).mkString != "") {
                        mapify((fr.findFirstIn(reqs)).mkString)
                    }
                }
            }
            // Handle properly. For now just ignore malformed requests.
            // ... and everything else evil too...
            catch { case e: Throwable => }
            rmap
        }
    }

    class OneConnection(sock: Socket) {
        val in  = new BufferedReader(new InputStreamReader(sock.getInputStream))
        val out = new PrintWriter(new OutputStreamWriter(sock.getOutputStream))

        def getRequests(): String = {
            val in  = new InputStreamReader(sock.getInputStream)
            val bos = new StringBuilder
            val ba  = new Array[Char](4096)
            val len = in.read(ba)
            if (len > 0) {
                bos.appendAll(ba, 0, len)
            }
            bos.toString
        }
    }
}
