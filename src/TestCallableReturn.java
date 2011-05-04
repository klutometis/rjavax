
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
import org.rosuda.REngine.REXPEnvironment;
import org.rosuda.REngine.REXPFactor;
import org.rosuda.REngine.REXPLanguage;
import org.rosuda.REngine.REXPList;
import org.rosuda.REngine.REXPRaw;
import org.rosuda.REngine.REXPReference;
import org.rosuda.REngine.REXPString;
import org.rosuda.REngine.REXPUnknown;
import org.rosuda.REngine.REXPWrapper;
import org.rosuda.REngine.REXPJavaReference;
import org.rosuda.REngine.JRI.JRIEngine;

public class TestCallableReturn {
    public static void test(REXPReference dollars,
                            REXPReference refClass,
                            String method) {
        try {
            // We can also get at the method followingly; but we run
            // into the problem that it is not a REXPReference and
            // therefore cannot, apparently, be evaluated:
            // 
            // System.out.println(((REXPEnvironment) refClass.getAttribute(".xData"))
            //                    .get("test"));

            REXPReference refClassMethod = (REXPReference)
                ((REXPJavaReference)
                 ((REXPReference) dollars).getEngine()
                 .eval(new REXPLanguage
                       (new RList(new REXP[] { dollars,
                                               refClass,
                                               new REXPString(method) })),
                       null,
                       true))
                .getObject();

            REXP value = refClassMethod.getEngine()
                .eval(new REXPLanguage
                      (new RList(new REXP[] { refClassMethod })),
                      null,
                      true);

            // Would be nice if we could actually extract callable
            // methods this way.
            assert ((REXPUnknown)
                    ((REXPEnvironment)
                     value.getAttribute(".xData")).get("test")).getType()
                == 3;
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
}
