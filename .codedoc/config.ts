
import { configuration } from '@codedoc/core';

import { theme } from './theme';


export const config = /*#__PURE__*/configuration({
  theme,                                  // --> add the theme. modify `./theme.ts` for changing the theme.
  page: {
    title: {
      base: 'Progen Cli'                  // --> the base title of your doc pages
    }
  },
  misc: {
    github: {
      user: 'dandro',
      repo: 'progen-cli',
      large: true,
    }
  },
  dest: {
    namespace: '/progen-cli'
  }
});
