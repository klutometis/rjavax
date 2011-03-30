
import java.lang.StringBuilder;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Formatter;

import org.rosuda.REngine.JRI.JRIEngine;
import org.rosuda.REngine.REngine;
import org.rosuda.REngine.REngineStdOutput;

import org.apache.commons.lang3.RandomStringUtils;

interface A {
    String b(String c, String d);
}

class TestInterfaceProxy {
    static REngine re;

    public static void main(String[] argv) throws Throwable {
        InvocationHandler handler = new InvocationHandler() {
                public Object invoke(Object proxy,
                                     Method method,
                                     Object[] args) {
                    try {
                        StringBuilder evalArgs = new StringBuilder();
                        for (Object arg: args) {
                            // http://stackoverflow.com/questions/41107/how-to-generate-a-random-alpha-numeric-string-in-java
                            String identifier = RandomStringUtils.randomAlphabetic(32);
                            // http://tolstoy.newcastle.edu.au/R/help/04/04/0847.html
                            re.assign(identifier, (String) arg);
                            evalArgs.append(identifier);
                            evalArgs.append(",");
                        };
                        if (evalArgs.length() > 0)
                            evalArgs.deleteCharAt(evalArgs.length() - 1);
                        return re.parseAndEval(String.format("%s(%s)",
                                                             method.getName(),
                                                             evalArgs)).asString();
                    } catch (Throwable e) {
                        e.printStackTrace();
                        // re.end();
                    }
                    System.out.println(method);
                    return new Object();
                };
            };
        A a = (A) Proxy.newProxyInstance(A.class.getClassLoader(),
                                         new Class[] { A.class },
                                         handler);
        
        re = JRIEngine.createEngine(new String[] { "--vanilla", "--slave" },
                                    new REngineStdOutput(),
                                    false);

        re.parseAndEval("b <- function(c, d) paste(c, d)");
 
        assert a.b("hello", "world").equals("hello world");
 
        re.close();
    }
}
