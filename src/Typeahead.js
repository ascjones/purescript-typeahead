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

exports.open = function(ob) {
  return function() {
    return ob.typeahead('open');
  }
};

exports.close = function(ob) {
  return function() {
    return ob.typeahead('close');
  }
};

exports.destroy = function(ob) {
  return function() {
    return ob.typeahead('destroy');
  }
};

exports.select = function(act) {
  return function(ta) {
    return function() {
      return ta.on('typeahead:select', function(e, suggestion) {
        act(e)(suggestion)();
      });
    }
  }
}
