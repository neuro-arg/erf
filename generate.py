import itertools

arithmetics = {
    "+",
    "*",
}

coercions = {
    "i16": ["u8","i8"],
    "i32": ["u16","i16"],
    "i64": ["u32","i32"],
    "u16": ["u8"],
    "u32": ["u16"],
    "u64": ["u32"],
    "f32": ["u64", "i64"],
    "f64": ["f32"],
}

allCoercions = {}

for k in coercions.keys():
    k0 = k
    c = [k]
    ks = [k]
    while ks:
        ks1 = ks
        ks = []
        for k in ks1:
            for k1 in coercions.get(k, []):
                if k1 not in c and k1 not in ks:
                    ks.append(k1)
        c.extend(ks)
    allCoercions[k0] = c

bools = ["bool"]
integers = ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"]
scalars = integers + ["f32", "f64"]

t1 = lambda t: [(x,) for x in t]
t2 = lambda t: [(x, x) for x in t]

ops = {
    "(+)": {
        "name": "add",
        "types": t2(scalars),
    },
    "(*)": {
        "name": "mul",
        "types": t2(scalars),
    },
    "(-)": {
        "name": "sub",
        "types": t2(scalars),
    },
    "(/)": {
        "name": "div",
        "types": t2(scalars),
    },
    "(//)": {
        "name": "quot",
        "types": t2(integers),
    },
    "(%)": {
        "name": "rem",
        "types": t2(scalars),
    },
    "(<<)": {
        "name": "shl",
        "types": t2(integers),
    },
    "(>>)": {
        "name": "shr",
        "types": t2(integers),
    },
    "(^)": {
        "name": "bitxor",
        "types": t2(integers),
    },
    "(&)": {
        "name": "bitand",
        "types": t2(integers),
    },
    "(|)": {
        "name": "bitor",
        "types": t2(integers),
    },
    "(**)": {
        "name": "pow",
        "types": t2(scalars),
    },
    # and, or aren't generic, they're control flow operators
    # eq, ne, lt, gt, le, ge
    "cmp": {
        "name": "cmp",
        "types": t2(scalars),
    },
    "!": {
        "name": "neg",
        "types": t1(bools),
    },
}

for ident, data in ops.items():
    name = data["name"]
    types = data["types"]
    if len(types) == 1:
        for ts in types:
            for t1 in allCoercions.get(ts[0], [ts[0]]):
                if t1 == ts[0]:
                    print(f'{name}(intrinsic::{t1} x) => intrinsic::{name}_{ts[0]} x;')
                else:
                    print(f'{name}(intrinsic::{t1} x) => intrinsic::{name}_{ts[0]}(intrinsic::{t1} x);')
    else:
        for ts in types:
            t1 = ts[0]
            for t2 in allCoercions.get(ts[1], [ts[1]]):
                if t1 == t2:
                    print(f'{ident}(intrinsic::{t1} x, intrinsic::{t2} y) => intrinsic::{name}_{t1}(x, y);')
                else:
                    print(f'{ident}(intrinsic::{t1} x, intrinsic::{t2} y) => intrinsic::{name}_{t1}(x, intrinsic::{t1} y);')
            t2 = ts[1]
            for t1 in allCoercions.get(ts[0], [ts[0]]):
                if t1 != t2:
                    print(f'{ident}(intrinsic::{t1} x, intrinsic::{t2} y) => intrinsic::{name}_{t2}(intrinsic::{t2} x, y);')
print('''
(<) (a, b) => let res(intrinsic::Less) => true; res(_) => false; in res(cmp(a, b));
(>=)(a, b) => let res(intrinsic::Less) => false; res(_) => true; in res(cmp(a, b));
(>) (a, b) => let res(intrinsic::Greater) => true; res(_) => false; in res(cmp(a, b));
(<=)(a, b) => let res(intrinsic::Greater) => false; res(_) => true; in res(cmp(a, b));
(==)(a, b) => let res(intrinsic::Equal) => true; res(_) => false; in res(cmp(a, b));
(!=)(a, b) => let res(intrinsic::Equal) => false; res(_) => true; in res(cmp(a, b));
''')
