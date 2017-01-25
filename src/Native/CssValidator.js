var _eeue56$sass_to_elm$Native_CssValidator = function () {
    var Nil = { ctor: '[]' };

    function Cons(hd, tl) {
      return { ctor: '::', _0: hd, _1: tl };
    }

    function fromArray(arr) {
      var out = Nil;
      for (var i = arr.length; i--; )
      {
        out = Cons(arr[i], out);
      }
      return out;
    }

    function toArray(xs) {
      var out = [];
      while (xs.ctor !== '[]')
      {
        out.push(xs._0);
        xs = xs._1;
      }
      return out;
    }

    var knownNames = fromArray([]);

    var isValid = function(name){
        console.log(this);
        return eval("typeof _rtfeldman$elm_css$Css$" + name) !== "undefined";
    };

    var levDistance = function(a, b){
        if(a.length == 0) return b.length;
        if(b.length == 0) return a.length;

        var matrix = [];

        // increment along the first column of each row
        var i;
        for(i = 0; i <= b.length; i++){
            matrix[i] = [i];
        }

      // increment each column in the first row
      var j;
      for(j = 0; j <= a.length; j++){
        matrix[0][j] = j;
      }

      // Fill in the rest of the matrix
      for(i = 1; i <= b.length; i++){
        for(j = 1; j <= a.length; j++){
          if(b.charAt(i-1) == a.charAt(j-1)){
            matrix[i][j] = matrix[i-1][j-1];
          } else {
            matrix[i][j] = Math.min(matrix[i-1][j-1] + 1, // substitution
                                    Math.min(matrix[i][j-1] + 1, // insertion
                                             matrix[i-1][j] + 1)); // deletion
          }
        }
      }

      return matrix[b.length][a.length];
    };

    return {
        isValid: isValid,
        names: knownNames,
        levDistance: F2(levDistance)
    };

}();
