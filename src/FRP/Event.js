export const foreachRefE = (ref) => (f) => () => {
    for (var i = 0; i < ref.value.length; i++) {
        f(ref.value[i])();
    }
}