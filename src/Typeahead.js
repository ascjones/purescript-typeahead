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
}
