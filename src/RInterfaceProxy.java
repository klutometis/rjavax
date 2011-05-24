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
    REXPReference dollars;
    REXPReference refClass;
    InvocationHandler handler;
    static HashMap<Class, Method> translators;

    public RInterfaceProxy(String interfaceName,
                           REXPReference dollars,
                           REXPReference refClass) {
        this.interfaceName = interfaceName;
        this.dollars = dollars;
        this.refClass = refClass;
        this.handler = makeInvocationHandler();
    }

    static {
        try {
            translators =
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
                    final REXPReference refClassMethod = (REXPReference)
                        ((REXPJavaReference)
                         ((REXPReference) dollars).getEngine()
                         .eval(new REXPLanguage
                               (new RList(new REXP[] {
                                       dollars,
                                       refClass,
                                       new REXPString(method.getName())
                                   })),
                               null,
                               true))
                        .getObject();

                    ArrayList<REXP> call = new ArrayList<REXP>() {
                        {
                            add(refClassMethod);
                            if (args != null) {
                                for (Object arg: args) {
                                    REXP wrappedArg = REXPWrapper.wrap(arg);
                                    if (wrappedArg != null)
                                        add(wrappedArg);
                                    else
                                        // Last-ditch attempt to add a
                                        // non-scalar
                                        add(new REXPJavaReference(arg));
                                }
                            }
                        }
                    };

                    REXP value = refClassMethod.getEngine()
                        .eval(new REXPLanguage
                              (new RList(call.toArray(new REXP[0]))),
                              null,
                              true);

                    if (value instanceof REXPJavaReference)
                        return ((REXPJavaReference) value).getObject();
                    else {
                        // This is not the inverse of REXPWrapper.wrap
                        // in the sense that we only support an
                        // inverse mapping of a subset of its domain:
                        Method translator = RInterfaceProxy.translators
                            .get(value.getClass());
                        if (translator != null)
                            return translator.invoke(value);
                        else
                            return null;
                    }
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
