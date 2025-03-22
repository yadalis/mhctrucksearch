 
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('app'),
  flags: "f_condition=1&f_model=T880&f_make=KW"
});

//registerServiceWorker();