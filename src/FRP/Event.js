export const fastForeachThunk = (as) => {
  for (var i = 0, l = as.length; i < l; i++) {
    as[i]();
  }
};

export const fastForeachE = (as, f) => {
  for (var i = 0, l = as.length; i < l; i++) {
    f(as[i]);
  }
};
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////

export const objHack = () => {
  return [new Map()];
};

export const insertObjHack = (k, v, o) => {
  o[o.length - 1].set(k, v);
};

export const deleteObjHack = (k, o) => {
  for (const m of o) {
    if (m.delete(k)) {
      return true;
    }
  }
  return false;
};

export const fastForeachOhE = (o, f) => {
  const M = new Map();
  const run = (i) => {
    o.push(new Map());
    o[i].forEach((v, k) => {
      f(v);
      if (o[i + 1].size) run(i + 1);
      o[i + 1].clear();
      M.set(k, v);
    });
  };
  run(0);
  o.length = 0;
  o.push(M);
};
