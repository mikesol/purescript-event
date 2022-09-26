export const fastForeachE = (as, f) => {
    for (var i = 0, l = as.length; i < l; i++) {
        f(as[i]);
    }
}