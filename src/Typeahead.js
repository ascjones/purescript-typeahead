/* global exports */
"use strict";

// module Typeahead

exports.typeahead = function(ob) {
  return function(opts) {
    return function(datasets) {
      return function() {
        return ob.typeahead(opts, datasets);
      };
    };
  };
};

exports.getVal = function(ob) {
  return function() {
    return ob.typeahead('val');
  }
};

exports.setVal = function(ob) {
  return function(val) {
    return function() {
      return ob.typeahead('val', val);
    }
  }
};
