
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
import org.rosuda.REngine.JRI.JRIEngine;

public class RInterfaceProxy {
    String interfaceName;
    HashMap<String, REXPReference> implementations;
    InvocationHandler handler;
    static HashMap<Class, Method> translations;

    public RInterfaceProxy(String interfaceName,
                           HashMap<String, REXPReference> implementations) {
        this.interfaceName = interfaceName;
        this.implementations = implementations;
        this.handler = makeInvocationHandler();
    }

    static {
        try {
            translations =
                new HashMap<Class, Method>() {
                {
                    put(REXPString.class,
                        REXPString.class.getMethod("asString"));
                    put(REXPRaw.class,
                        REXPRaw.class.getMethod("asBytes"));
                    put(REXPDouble.class,
                        REXPDouble.class.getMethod("asDouble"));
                    put(REXPFactor.class,
                        REXPFactor.class.getMethod("asFactor"));
                    put(REXPInteger.class,
                        REXPInteger.class.getMethod("asInteger"));
                    put(REXPList.class,
                        REXPList.class.getMethod("asList"));                    
                }
            };
        } catch (Throwable e) {
            e.printStackTrace();
        }
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
                            for (Object arg: args) {
                                add(REXPWrapper.wrap(arg));
                            }
                        }
                    };
                    REXP value = implementation.getEngine()
                        .eval(new REXPLanguage
                              (new RList (call.toArray(new REXP[0]))),
                            null,
                            true);
                    // NB: need to handle the case where get returns
                    // null.
                    return RInterfaceProxy.translations
                        .get(value.getClass()).invoke(value);
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
