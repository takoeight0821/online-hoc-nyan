
var postCompiler = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/compiler'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}
