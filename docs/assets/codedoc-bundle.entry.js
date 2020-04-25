import { getRenderer } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/transport/renderer.js';
import { initJssCs } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/transport/setup-jss.js';initJssCs();
import { installTheme } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/content/theme.ts';installTheme();
import { codeSelection } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/code/selection.js';codeSelection();
import { sameLineLengthInCodes } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/code/same-line-length.js';sameLineLengthInCodes();
import { initHintBox } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/code/line-hint/index.js';initHintBox();
import { initCodeLineRef } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/code/line-ref/index.js';initCodeLineRef();
import { initSmartCopy } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/code/smart-copy.js';initSmartCopy();
import { copyHeadings } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/heading/copy-headings.js';copyHeadings();
import { contentNavHighlight } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/page/contentnav/highlight.js';contentNavHighlight();
import { loadDeferredIFrames } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/transport/deferred-iframe.js';loadDeferredIFrames();
import { smoothLoading } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/transport/smooth-loading.js';smoothLoading();
import { tocHighlight } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/page/toc/toc-highlight.js';tocHighlight();
import { postNavSearch } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/page/toc/search/post-nav/index.js';postNavSearch();
import { ToCPrevNext } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/page/toc/prevnext/index.js';
import { GithubSearch } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/misc/github/search.js';
import { ToCToggle } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/page/toc/toggle/index.js';
import { DarkModeSwitch } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/darkmode/index.js';
import { ConfigTransport } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/transport/config.js';
import { TabSelector } from '/Users/daniel.martinez/Documents/haskell/progen-cli/.codedoc/node_modules/@codedoc/core/dist/es6/components/tabs/selector.js';

const components = {
  'zyEsvuaLkfrWq2QfzDRHOA==': ToCPrevNext,
  'wCR4oefqeEqXKXSlPABG/g==': GithubSearch,
  '3pUrTu0nD7QHTKxdCVhO6g==': ToCToggle,
  'K1ctEvy9jvqcm0mVwtUqQw==': DarkModeSwitch,
  'H+BOVL1d6Jlaf+t5vK1DyQ==': ConfigTransport,
  'zAY18kfbF7IL9RRY57V69Q==': TabSelector
};

const renderer = getRenderer();
const ogtransport = window.__sdh_transport;
window.__sdh_transport = function(id, hash, props) {
  if (hash in components) {
    const target = document.getElementById(id);
    renderer.render(renderer.create(components[hash], props)).after(target);
    target.remove();
  }
  else if (ogtransport) ogtransport(id, hash, props);
}
