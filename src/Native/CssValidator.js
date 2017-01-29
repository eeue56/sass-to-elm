var script = document.currentScript;
console.log(document.currentScript.innerHTML)
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

    var elm_css_regex = /var _rtfeldman\$elm_css\$Css\$(.+?)\=/g;

    var findNames = function(stuff){
      stuff.match(elm_css_regex).map(function(v){
        var result = /var _rtfeldman\$elm_css\$Css\$(.+?)\=/g.exec(v)[1];
        result = result.trim();
      });
    };

    var loadNodes = function(){
      console.log(script.innerHTML)
      console.log(document.getElementsByTagName('script'));
    }

    var isValid = function(name){
      loadNodes();
        return eval("typeof _rtfeldman$elm_css$Css$" + name) !== "undefined";
    };

    var numberOfArgs = function(name){
        return eval("_rtfeldman$elm_css$Css$" + name + ".length");
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
        numberOfArgs: numberOfArgs,
        isValid: isValid,
        names: knownNames,
        levDistance: F2(levDistance)
    };

}();
