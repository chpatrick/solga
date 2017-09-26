// taken from
// <https://github.com/ghcjs/ghcjs-base/blob/eacf95aac3061275699563e1802eabe4a8f4aaec/jsbits/xhr.js>
// see that page for license

function h$solgaSendXHR(xhr, d, cont) {
  xhr.addEventListener('error', function () {
    cont(2);
  });
  xhr.addEventListener('abort', function() {
    cont(1);
  });
  xhr.addEventListener('load', function() {
    cont(0);
  });
  if (d) {
    xhr.send(d);
  } else {
    xhr.send();
  }
}

