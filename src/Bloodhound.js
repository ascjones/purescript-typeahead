/* global exports */
"use strict";

// module Bloodhound

exports.typeahead = function(ob) {
  return function(opts) {
    return function(datasets) {
      return function() {
        return ob.typeahead(opts, datasets);
      };
    };
  };
};
