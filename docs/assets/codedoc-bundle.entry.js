import { getRenderer } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\transport\renderer.js';
import { initJssCs } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\transport\setup-jss.js';initJssCs();
import { installTheme } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\content\theme.ts';installTheme();
import { codeSelection } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\code\selection.js';codeSelection();
import { sameLineLengthInCodes } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\code\same-line-length.js';sameLineLengthInCodes();
import { initHintBox } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\code\line-hint\index.js';initHintBox();
import { initCodeLineRef } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\code\line-ref\index.js';initCodeLineRef();
import { initSmartCopy } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\code\smart-copy.js';initSmartCopy();
import { copyHeadings } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\heading\copy-headings.js';copyHeadings();
import { contentNavHighlight } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\page\contentnav\highlight.js';contentNavHighlight();
import { loadDeferredIFrames } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\transport\deferred-iframe.js';loadDeferredIFrames();
import { smoothLoading } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\transport\smooth-loading.js';smoothLoading();
import { tocHighlight } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\page\toc\toc-highlight.js';tocHighlight();
import { postNavSearch } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\page\toc\search\post-nav\index.js';postNavSearch();
import { ToCPrevNext } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\page\toc\prevnext\index.js';
import { ToCToggle } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\page\toc\toggle\index.js';
import { DarkModeSwitch } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\darkmode\index.js';
import { ConfigTransport } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\transport\config.js';
import { TabSelector } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\tabs\selector.js';
import { CollapseControl } from 'C:\Users\dandr\Documents\haskell\progen-cli\.codedoc\node_modules\@codedoc\core\dist\es5\components\collapse\collapse-control.js';

const components = {
  'mp1S5REG0yL+0UY5wSNVtw==': ToCPrevNext,
  'StfpvT3XJiJwOYx8qjUoBA==': ToCToggle,
  'FrSj+/Csw0+daw8vQ/KBnQ==': DarkModeSwitch,
  'UuBq8SUOaPGsWGPAsaC8FQ==': ConfigTransport,
  'jDzn4CDwT6TYma+3TYr2NQ==': TabSelector,
  'wKO6JZUUhVsGIG5tB/rjLw==': CollapseControl
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
