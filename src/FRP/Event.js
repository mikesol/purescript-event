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

export const objHack = () => ({});

export const insertObjHack = (k,v,o) => { o[k] = v; }

export const deleteObjHack = (k,o) => { delete o[k]; }
