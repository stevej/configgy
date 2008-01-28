package net.lag.configgy

import java.util.regex._


object StringUtils {
    /*
     * Scala does not yet (Dec 2007) support java's String.format natively.
     * Fake it by building the Object[] manually for a handful of params.
     */
    def format(s: String, items: Any*) = String.format(s, (for (i <- items) yield i.asInstanceOf[Object]).toArray)

    
    // emulate python re.sub
    def patternSub(p: Pattern, s: String, replace: (Matcher => String)) = {
        var offset = 0
        var out = new StringBuilder
        val m = p.matcher(s)
        
        while (m.find()) {
            if (m.start > offset) {
                out.append(s.substring(offset, m.start))
            }
        
            out.append(replace(m))
            offset = m.end
        }
    
        if (offset < s.length) {
            out.append(s.substring(offset))
        }
        out.toString
    }
    
    
    private val QUOTE_RE = Pattern.compile("[\u0000-\u001f\u007f-\uffff\\\\\"]")

    def quoteC(s: String) = {
        patternSub(QUOTE_RE, s, (m: Matcher) => {
            m.group.charAt(0) match {
                case '\r' => "\\r"
                case '\n' => "\\n"
                case '\t' => "\\t"
                case '"' => "\\\""
                case '\\' => "\\\\"
                case c => {
                    if (c <= 255) {
                        format("\\x%02x", c.asInstanceOf[Int])
                    } else {
                        format("\\u%04x", c.asInstanceOf[Int])
                    }
                }
            }
        })
    }


    private val UNQUOTE_RE = Pattern.compile("\\\\(u[\\dA-Fa-f]{4}|x[\\dA-Fa-f]{2}|[^ux])")

    def unquoteC(s: String) = {
        patternSub(UNQUOTE_RE, s, (m: Matcher) => {
            val ch = m.group(1).charAt(0) match {
                // holy crap! this is terrible:
                case 'u' => Character.valueOf(Integer.valueOf(m.group(1).substring(1), 16).asInstanceOf[Int].toChar)
                case 'x' => Character.valueOf(Integer.valueOf(m.group(1).substring(1), 16).asInstanceOf[Int].toChar)
                case 'r' => '\r'
                case 'n' => '\n'
                case 't' => '\t'
                case x => x
            }
            ch.toString
        })
    }
    
    // FIXME: what?
}
