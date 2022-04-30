export function actualizeMMZ_(mmz) {
	return function () {
		if (!mmz.ctx.start.actualized) {
			mmz.ctx.start.actualized = true;
			mmz.ctx.start.event()();
		}
	};
}

export function mmzStart_(e) {
	const next = {
		type: "start",
		value: undefined,
		cbs: [],
		memo: undefined,
		actualized: false,
		event: e,
		depth: 0,
		ctx: [[]],
	};
	next.ctx[0][0] = next;
	next.ctx.start = next;
	return next;
}

export function mmzMap(ab) {
	return function (fa) {
		const depth = fa.depth + 1;
		const depthM1 = depth - 1;
		const next = {
			type: "map",
			value: { ab: ab, fa: fa },
			cbs: [],
			depth: depth,
			memo: undefined,
			ctx: fa.ctx,
		};
		if (fa.ctx) {
			if (fa.ctx[depthM1] === undefined) {
				fa.ctx[depthM1] = [];
			}
			fa.ctx[depthM1].push(next);
		}
		return next;
	};
}

export function mmzBang(a) {
	return {
		type: "bang",
		value: a,
		depth: 0,
		cbs: [],
		memo: [a],
	};
}

export function mmzAlt(fa) {
	return function (fb) {
		const depth = Math.max(fa.depth, fb.depth) + 1;
		const depthM1 = depth - 1;
		const next = {
			type: "alt",
			value: { fa: fa, fb: fb },
			cbs: [],
			depth: depth,
			memo: undefined,
			ctx: fa.ctx || fb.ctx,
		};
		if (next.ctx) {
			if (next.ctx[depthM1] === undefined) {
				next.ctx[depthM1] = [];
			}
			next.ctx[depthM1].push(next);
		}
		return next;
	};
}

export const mmzEmpty = {
	type: "empty",
	cbs: [],
	depth: 0,
	memo: [],
};

export function mmzKeepLatest(ffa) {
	return ffa.value;
}

export function mmzSampleOn(fa) {
	return function (fab) {
		const depth = Math.max(fab.depth, fa.depth) + 1;
		const depthM1 = depth - 1;
		const next = {
			type: "sampleOn",
			value: { fab: fab, fa: fa },
			cbs: [],
			depth: depth,
			memo: undefined,
			ctx: fa.ctx || fab.ctx,
		};
		if (next.ctx) {
			if (next.ctx[depthM1] === undefined) {
				next.ctx[depthM1] = [];
			}
			next.ctx[depthM1].push(next);
		}
		return next;
	};
}

export function mmzPartitionMap(aelr) {
	return function (fa) {
		const depth = fa.depth + 1;
		const depthM1 = depth - 1;
		const left = {
			type: "partitionMapLeft",
			value: { aelr: aelr, fa: fa },
			cbs: [],
			depth: depth,
			memo: undefined,
			ctx: fa.ctx,
		};
		const right = {
			type: "partitionMapRight",
			value: { aelr: aelr, fa: fa },
			cbs: [],
			depth: depth,
			memo: undefined,
			ctx: fa.ctx,
		};
		if (fa.ctx) {
			if (fa.ctx[depthM1] === undefined) {
				fa.ctx[depthM1] = [];
			}
			fa.ctx[depthM1].push(left);
			fa.ctx[depthM1].push(right);
		}
		return { left: left, right: right };
	};
}

export function mmzFold(abb) {
	return function (fa) {
		return function (b) {
			const depth = fa.depth + 1;
			const depthM1 = depth - 1;
			const next = {
				type: "fold",
				value: { abb: abb, fa: fa, b: b },
				cbs: [],
				depth: depth,
				memo: undefined,
				ctx: fa.ctx,
			};
			if (fa.ctx) {
				if (fa.ctx[depthM1] === undefined) {
					fa.ctx[depthM1] = [];
				}
				fa.ctx[depthM1].push(next);
			}
			return next;
		};
	};
}

const runMMZInternal = function (opts, ctx) {
	for (var ij = 0; ij < ctx.length; ij++) {
		for (var ix = 0; ix < ctx[ij].length; ix++) {
			var mmz = ctx[ij][ix];
			if (mmz.type === "start") {
				mmz.memo = [mmz.value];
			} else if (mmz.type === "bang") {
			} else if (mmz.type === "map") {
				mmz.memo = [];
				for (var i = 0; i < mmz.value.fa.memo.length; i++) {
					mmz.memo.push(mmz.value.ab(mmz.value.fa.memo[i]));
				}
			} else if (mmz.type === "sampleOn") {
				mmz.memo = [];
				for (var i = 0; i < mmz.value.fab.memo.length; i++) {
					for (var j = 0; j < mmz.value.fa.memo.length; j++) {
						mmz.memo.push(mmz.value.fab.memo[i](mmz.value.fa.memo[j]));
					}
				}
			} else if (mmz.type === "alt") {
				mmz.memo = [];
				for (var i = 0; i < mmz.value.fa.memo.length; i++) {
					mmz.memo.push(mmz.value.fa.memo[i]);
				}
				for (var i = 0; i < mmz.value.fb.memo.length; i++) {
					mmz.memo.push(mmz.value.fb.memo[i]);
				}
			} else if (mmz.type === "empty") {
				mmz.memo = [];
			} else if (mmz.type === "partitionMapLeft") {
				var l = [];
				var e = opts.either((x) => l.push(x))(() => {});
				for (var i = 0; i < mmz.value.fa.memo.length; i++) {
					e(mmz.value.aelr(mmz.value.fa.memo[i]));
				}
				mmz.memo = l;
			} else if (mmz.type === "partitionMapRight") {
				var r = [];
				var e = opts.either(() => {})((x) => r.push(x));
				for (var i = 0; i < mmz.value.fa.memo.length; i++) {
					e(mmz.value.aelr(mmz.value.fa.memo[i]));
				}
				mmz.memo = r;
			} else if (mmz.type === "fold") {
				mmz.memo = [];
				var b = mmz.value.b;
				for (var i = 0; i < mmz.value.fa.memo.length; i++) {
					b = mmz.value.abb(mmz.value.fa.memo[i])(b);
					mmz.memo.push(b);
				}
				mmz.value.b = b;
			} else {
				throw new Error("Failed pattern match on mmz", mmz);
			}
			if (mmz.type !== "empty") {
				for (var i = 0; i < mmz.cbs.length; i++) {
					for (var j = 0; j < mmz.memo.length; j++) {
						mmz.cbs[i](mmz.memo[j])();
					}
				}
			}
		}
	}
};

export function addSubscription_(sub) {
	return function (mmz) {
		return function () {
			if (mmz.type !== "empty") {
				mmz.cbs.push(sub);
				if (mmz.memo) {
					for (var j = 0; j < mmz.memo.length; j++) {
						sub(mmz.memo[j])();
					}
				}
			}
		};
	};
}

export function removeSubscription_(sub) {
	return function (mmz) {
		return function () {
			if (mmz.type !== "empty") {
				mmz.cbs.filter(function (x) {
					return x !== sub;
				});
			}
		};
	};
}

export function runMMZ_(opts) {
	return function (r) {
		return function (mmz) {
			return function () {
				for (var i = 0; i < mmz.ctx.length; i++) {
					mmz.ctx[i].memo = undefined;
				}
				mmz.ctx.start.value = r;
				runMMZInternal(opts, mmz.ctx);
			};
		};
	};
}
