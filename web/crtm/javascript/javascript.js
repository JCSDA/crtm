EventUtil.addEventHandler(window, 'load', onWindowLoad);

//------------------------------------------
function onWindowLoad() {
  var category = document.getElementById('category').getAttribute('value');
  var current_category = document.getElementById(category).setAttribute('class', 'current_category');
}
