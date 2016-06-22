var parser = null;

function getStats(data) {
  return {
    nodes: parser.getAST(data.ast),
    errors: parser.getErrors()
  }
}

onmessage = function(event) {
  switch (event.data.kind) {
    case 'init':
      if (!parser) {
        importScripts('./target/scala-2.11/papa-carlo-opt.js');
        parser = DtLangApi();
      }
      postMessage({kind: 'ready'});
      break;

    case 'fragment':
      postMessage({
        kind: 'fragment',
        response: parser.getNodeFragment(event.data.id)
      });
      break;

    case 'input':
      var start = new Date().getTime();
      parser.inputAll(event.data.code);
      var end = new Date().getTime();
      postMessage({
        kind: 'response',
        delta: end - start,
        stats: getStats(event.data)
      });
      break;
  }
};

