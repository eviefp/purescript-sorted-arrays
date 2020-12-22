exports.sortedArrayConcatImpl = function(lessThan) {
    return function(xs) {
        return function(ys) {
            if (xs.length === 0) return ys;
            if (ys.length === 0) return xs;

            var i = 0, j = 0, res = [];

            while(i < xs.length && j < ys.length) {
                if(lessThan(xs[i])(ys[j])) {
                    res.push(xs[i++]);
                } else {
                    res.push(ys[j++]);
                }
            }

            while(i < xs.length) {
                res.push(xs[i++]);
            }
            while(j < ys.length) {
                res.push(ys[j++]);
            }

            return res;
        };
    };
};
