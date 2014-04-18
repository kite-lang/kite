
var print = function (str) { console.log(str); };
var length = function (xs) { return xs.length; };
var slice = function (xs){
    return function(from) {
        return function(to) {
            return xs.slice(from, to);
        };
    };
};

var KT_COLON = function (x) {
    return function (xs) {
        /* TODO: this isn't a deep clone */
        var clone = xs.slice(0);
        clone.unshift(x);
        return clone;
    };
};

var KT_PLUS = function  (l) { return function (r) { return l + r; };};
var KT_MINUS = function  (l) { return function (r) { return l - r; };};
var KT_STAR = function  (l) { return function (r) { return l * r;};};
var KT_SLASH = function  (l) { return function (r) { return l / r;};};

var KT_PERCENT = function  (l) { return function (r) { return l % r;};};

var KT_POUND = function  (arr) { return function (idx) { return arr[idx];}; };

var KT_EQKT_EQ = function  (l) { return function (r) { return l === r;};};
var KT_LTKT_EQ = function  (l) { return function (r) { return l <= r;};};


var KT_IF = function (cond) {
    return function (conseq) {
        return function (alt) {
            if (cond()) return conseq(); else return alt();
        };
    };
};
