import java.io._
import java.net._
import util.matching.Regex
import collection.mutable.HashMap

object proforma {
    def main(args: Array[String]) {

        val file  = "htmls/index.html"
        val src   = io.Source.fromFile(file)
        val html  = src.getLines.mkString("\n")
        src.close()

        val srvCntLen = html.length()

        val port = 18080

        try {
            val ssock = new ServerSocket(port)
            println(s"$port has been opened")

            while(true) {
                val sock   = ssock.accept()
                println("Client has made socket connection")
                val client = new OneConnection(sock)
                val reqs   = client.getRequests()

                print(reqs)

                forms(reqs).foreach{case(k,v)=>println(s"$k : $v")}

                client.out.print("HTTP/1.1 200 OK\r\n")
                client.out.print(s"Content-Length: $srvCntLen\r\n")
                client.out.print(s"Content-type: text/html\r\n\r\n$html")
                client.out.flush()

                sock.close()
            }
        }
        catch {
            case e: IOException =>
                System.err.println(s"Could not listen on port $port")
                System.exit(1)
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
