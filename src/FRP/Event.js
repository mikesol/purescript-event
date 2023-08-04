export const fastForeachThunk = (as) => {
    for (var i = 0, l = as.length; i < l; i++) {
        as[i]();
    }
}

export const fastForeachE = (as, f) => {
    for (var i = 0, l = as.length; i < l; i++) {
        f(as[i]);
    }
}

export const fastForeachOhE = (o, f) => {
  for (const a in o) {
      f(o[a]);
  }
}

export const insertObjHack = (k,v,o) => {
  m.set(k, v);
};
export const deleteObjHack = (k, m) => {
  return m.delete(k);
};
export const objHack = () => {
  return new Map();
};
