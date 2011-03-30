
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.rosuda.REngine.RList;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.REXPReference;
import org.rosuda.REngine.REXPLanguage;

public class RInterfaceProxy {
    public String interfaceName;
    public HashMap<String, REXPReference> implementations;
    public InvocationHandler handler;

    public RInterfaceProxy(String interfaceName,
                           HashMap<String, REXPReference> implementations) {
        this.interfaceName = interfaceName;
        this.implementations = implementations;
        this.handler = makeInvocationHandler();
    }

    public InvocationHandler makeInvocationHandler() {
        return new InvocationHandler() {
            public Object invoke(Object proxy,
                                 Method method,
                                 Object[] args) {
                try {
                    REXPReference implementation =
                        RInterfaceProxy.this.implementations
                        .get(method.getName());
                    return implementation.getEngine()
                        .eval(new REXPLanguage(new RList(new REXP[] {
                                        implementation,
                                        (REXP) args[0]
                                    })),
                            null,
                            // docs show false
                            true);
                } catch (Throwable e) {
                    e.printStackTrace();
                }
                return null;
            }
        };
    }

    public Object newInstance() {
        try {
            Class interfaceClass = Class.forName(interfaceName);
            return Proxy.newProxyInstance(interfaceClass.getClassLoader(),
                                          new Class[] { interfaceClass },
                                          handler);
        } catch (Throwable e) {
            e.printStackTrace();
        }

        return null;
    }
}
