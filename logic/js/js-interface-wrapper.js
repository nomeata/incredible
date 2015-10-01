/*
 * GHCJS does not allow to export functions that return a value, so
 * this is a work-around-wrapper.
 *
 * TODO: Proper error reporting. How is it done usually?
 */

function incredibleLogic(context, proof) {
    var tmp = {"context": context, "proof": proof};
    incredibleLogic_(tmp);
    if (tmp.analysis) {
        return tmp.analysis;
    } else {
        return tmp.error;
    }
}

function incredibleNewRule(context, proof) {
    var tmp = {"context": context, "proof": proof};
    incredibleNewRule_(tmp);
    if (tmp.error) {
        return tmp.error;
    } else {
        return tmp.rule;
    }
}

function incredibleFormatTerm(input) {
    var tmp = {"prop": input};
    incredibleFormatTerm_(tmp);
    if (tmp.error) {
        return null;
    } else {
        return tmp.result;
    }
}
