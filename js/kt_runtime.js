var _print = function (str) {
    var out = str;
    if (KT_isArray(str) && str.length > 0) {
        if (Object.prototype.toString.call(str[0]) === '[object String]') {
            out = str.join('');
        } else {
            for (var i = 0; i < str.length; i++) {
                out[i] = _print(str[i]);
            }
        }
    }
    return out;
};

var print = function (str) {
    console.log(_print(str));
};

var show = function (expr) {
    return new String(_print(expr)).split('');
};

var put = function (s) { process.stdout.write(s); };

var panic = function (s) {
    console.log("Panic: " + _print(s));
    process.exit();
};

/* Math */
var sqrt = Math.sqrt;
var sin = Math.sin;
var cos = Math.cos;
var tan = Math.tan;

var random = function(b){ return function (a) {
    return Math.floor(Math.random() * a + b);
};};

/* Utils */
function sleep(time) {
    var stop = new Date().getTime();
    while(new Date().getTime() < stop + time) { ; }
}

function clear() {
    process.stdout.write('\u001B[2J\u001B[0;0f');
}

var KT_PLUS = function  (l) { return function (r) { return l + r; };};
var KT_MINUS = function  (l) { return function (r) { return l - r; };};
var KT_STAR = function  (l) { return function (r) { return l * r;};};
var KT_SLASH = function  (l) { return function (r) { return l / r;};};

var KT_PERCENT = function  (l) { return function (r) { return l % r;};};
var KT_XOR = function (l) { return function (r) { return l ^ r ;};};

var KT_LTKT_EQ = function  (l) { return function (r) { return l <= r;};};
var KT_EQKT_EQ = function  (l) { return function (r) {
    /* check type of lhs only as types are trusted */
    if (KT_isArray(l)) {
        /* early return if both not list  */
        if (!KT_isArray(r)) return false;

        var llen = l.length;
        var rlen = r.length;
        /* early return if lengths differ  */
        if (llen != rlen) return false;
        /* recursively match each element */
        var eq = true;
        for (var i = 0; i < llen; i++) {
            eq = eq && KT_EQKT_EQ(l[i])(r[i]);
        }
        return eq;
    }
    else return l === r;
};};

var KT_COLON = function (x) {
    return function (xs) {
        var clone = xs.slice(0);
        clone.unshift(x);
        return clone;
    };
};

var KT_isArray = function (e) {
    return Object.prototype.toString.call(e) === '[object Array]';
};

var KT_if = function (cond) {
    return function (conseq) {
        return function (alt) {
            if (cond()) return conseq(); else return alt();
        };
    };
};

var KT_match = function (val, patterns) {
    for (var i = 0; i < patterns.length; i++) {
        var pattern = patterns[i];
        switch (pattern.type) {
        case 'simple':
            if (KT_EQKT_EQ(val)(pattern.expr())) return pattern.conseq();
            break;

        case 'list':
            if (KT_isArray(val) && val.length > 0) {
                return pattern.conseq(val[0], val.slice(1));
            }
            break;

        case 'pair':
            return pattern.conseq(val[0], val[1]);

        case 'otherwise':
            return pattern.conseq();
            break;
        }
    }

    throw "Non-exhaustive pattern" + ", pattern.type: " + pattern.type + ", val: " + val;
};

var KT_arguments = function (none) {
    return process.argv;
};
