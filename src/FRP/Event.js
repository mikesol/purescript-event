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
  return { r: false, q: [], m: [new Map()] };
};

export const insertObjHack = (k, v, o) => {
  o.m[o.m.length - 1].set(k, v);
};

export const deleteObjHack = (k, o) => {
  for (const m of o.m) {
    if (m.delete(k)) {
      return true;
    }
  }
  return false;
};

export const fastForeachOhE = (o, f) => {
  if (o.r) {
    o.q.push(() => {fastForeachOhE(o, f)});
    return;
  }
  o.r = true;
  const M = new Map();
  const run = (i) => {
    o.m.push(new Map());
    o.m[i].forEach((v, k) => {
      f(v);
      if (o.m[i + 1].size) run(i + 1);
      o.m[i + 1].clear();
      o.m.length = i + 1 + 1;
      M.set(k, v);
    });
  };
  run(0);
  o.m.length = 0;
  o.m.push(M);
  let fn;
  o.r = false;
  while (fn = o.q.shift()) {
    fn();
  }
};
