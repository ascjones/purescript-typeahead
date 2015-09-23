/* global exports */
"use strict";

// module Typeahead

exports.typeahead = function(options, datasets) {
    return function(ob) {
        return function() {
            return ob.typeahead(options, datasets);
        };
    };
}
