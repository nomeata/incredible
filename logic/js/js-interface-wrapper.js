/*
 * GHCJS does not allow to export functions that return a value, so
 * this is a work-around-wrapper.
 *
 * TODO: Proper error reporting. How is it done usually?
 */

function incredibleLogic(context, task, proof) {
    var tmp = {"context": context, "task": task, "proof": proof};
    incredibleLogic_(tmp);
    if (tmp.analysis) {
        return tmp.analysis;
    } else {
        return tmp.error;
    }
}
