import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.rosuda.REngine.RList;
import org.rosuda.REngine.REngine;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.REXPDouble;
import org.rosuda.REngine.REXPInteger;
import org.rosuda.REngine.REXPFactor;
import org.rosuda.REngine.REXPLanguage;
import org.rosuda.REngine.REXPList;
import org.rosuda.REngine.REXPRaw;
import org.rosuda.REngine.REXPReference;
import org.rosuda.REngine.REXPString;
import org.rosuda.REngine.REXPWrapper;
import org.rosuda.REngine.REXPJavaReference;
import org.rosuda.REngine.JRI.JRIEngine;

public class RInterfaceProxy {
    String interfaceName;
    HashMap<String, REXPReference> implementations;
    InvocationHandler handler;

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
                                 final Object[] args) {
                try {
                    final REXPReference implementation =
                        RInterfaceProxy.this.implementations
                        .get(method.getName());
                    ArrayList<REXP> call = new ArrayList<REXP>() {
                        {
                            add(implementation);
                            if (args != null) {
                                for (Object arg: args) {
                                    add(new REXPJavaReference(arg));
                                }
                            }
                        }
                    };
                    REXP value = implementation.getEngine()
                        .eval(new REXPLanguage
                              (new RList (call.toArray(new REXP[0]))),
                              null,
                              true);
                    // I've seen a REXPNull here before; am I masking
                    // an error, though?
                    if (value instanceof REXPJavaReference)
                        return ((REXPJavaReference) value).getObject();
                    else
                        return null;
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
