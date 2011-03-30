
import org.rosuda.REngine.REXP;

public interface Mogrifier {
    // HACK: I really want to be giving and taking a String, not an
    // REXP and an Object!
    public REXP mogrify(Object string);
}
