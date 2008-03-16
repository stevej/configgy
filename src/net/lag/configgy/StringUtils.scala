package net.lag.configgy

import java.util.regex._


/**
 * Helpful utility functions for dealing with strings.
 */
object StringUtils {
    /**
     * Scala does not yet (Dec 2007) support java's String.format natively.
     * Fake it by building the Object[] manually for a handful of params.
     */
    def format(s: String, items: Any*) = String.format(s, items.toArray.asInstanceOf[Array[Object]])

    
    /**
     * For every section of a string that matches a regular expression, call
     * a function to determine a replacement (as in python's
     * <code>re.sub</code>). The function will be passed the Matcher object
     * corresponding to the substring that matches the pattern, and that
     * substring will be replaced by the function's result.
     *
     * <p> For example, this call:
     *
     * <p><code> patternSub(Pattern.compile("h."), "ohio", m => "n") </code>
     *
     * <p> will return the string <code>"ono"</code>.
     *
     * <p> The matches are found using <code>Matcher.find()</code> and so
     * will obey all the normal java rules (the matches will not overlap,
     * etc).
     *
     * @param p the regex pattern to replace
     * @param s the string to perform replacement on
     * @param replace a function that takes Matcher objects and returns a
     *     string to substitute
     * @return the resulting string with replacements made
     */
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

    /**
     * Quote a string so that unprintable chars (in ASCII) are represented by
     * C-style backslash expressions. For example, a raw linefeed will be
     * translated into <code>"\n"</code>. Control codes (anything below 0x20)
     * and unprintables (anything above 0x7E) are turned into either
     * <code>"\xHH"</code> or <code>"\\uHHHH"</code> expressions, depending on
     * their range. Embedded backslashes and double-quotes are also quoted.
     *
     * @param s the string to quote
     * @return a quoted string, suitable for ASCII display
     */
    def quoteC(s: String) = {
        patternSub(QUOTE_RE, s, m => {
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


    // we intentionally don't unquote "\$" here, so it can be used to escape interpolation later.
    private val UNQUOTE_RE = Pattern.compile("\\\\(u[\\dA-Fa-f]{4}|x[\\dA-Fa-f]{2}|[rnt\"\\\\])")

    /**
     * Unquote an ASCII string that has been quoted in a style like
     * {@link #quoteC(String)} and convert it into a standard unicode string.
     * <code>"\\uHHHH"</code> and <code>"\xHH"</code> expressions are unpacked
     * into unicode characters, as well as <code>"\r"</code>, <code>"\n"<code>,
     * <code>"\t"</code>, <code>"\\"<code>, and <code>'\"'</code>.
     *
     * @param s the ASCII string to unquote
     * @return an unquoted unicode string
     */
    def unquoteC(s: String) = {
        patternSub(UNQUOTE_RE, s, m => {
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
}
