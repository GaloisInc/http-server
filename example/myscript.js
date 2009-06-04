$(document).ready(function(){
  jQuery.ajax({ url:  '/'
              , cache: false
              , data: '{}'
              , type: 'POST'
              , dataType: 'json'
              , contentType: 'application/json'
              , success: function(d,s){alert(d.success);}
              });
});

