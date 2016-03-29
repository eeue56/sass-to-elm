var make = function make(elm) {
    elm.Native = elm.Native || {};
    elm.Native.CssValidator = elm.Native.CssValidator || {};

    if (elm.Native.CssValidator.values) return elm.Native.CssValidator.values;

    var List = Elm.Native.List.make(elm);
    var Css = Elm.Css.make(elm);
    var names = Object.keys(Css);

    var isValid = function(name){
        return names.indexOf(name) > -1;
    };

    var knownNames = List.fromArray(names);

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

    return elm.Native.CssValidator.values = {
        isValid: isValid,
        names: knownNames,
        levDistance: F2(levDistance)
    };
};

Elm.Native.CssValidator = {};
Elm.Native.CssValidator.make = make;
