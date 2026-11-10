import { withMermaid } from 'vitepress-plugin-mermaid'
import apiSidebar from '../api/_sidebar.json'

export default withMermaid({
  title: 'FiNeR Documentation',
  base: '/FiNeR/',
  head: [
    ['link', { rel: 'icon', type: 'image/svg+xml', href: '/FiNeR/favicon.svg' }],
    ['link', { rel: 'icon', type: 'image/png', sizes: '32x32', href: '/FiNeR/favicon-32.png' }],
    ['link', { rel: 'icon', type: 'image/png', sizes: '16x16', href: '/FiNeR/favicon-16.png' }],
    ['link', { rel: 'apple-touch-icon', sizes: '180x180', href: '/FiNeR/apple-touch-icon.png' }],
    ['meta', { name: 'theme-color', media: '(prefers-color-scheme: light)', content: '#f8f8f5' }],
    ['meta', { name: 'theme-color', media: '(prefers-color-scheme: dark)', content: '#121620' }],
    ['link', { rel: 'preconnect', href: 'https://fonts.googleapis.com' }],
    ['link', { rel: 'preconnect', href: 'https://fonts.gstatic.com', crossorigin: '' }],
    ['link', { rel: 'stylesheet', href: 'https://fonts.googleapis.com/css2?family=Shippori+Mincho:wght@800&family=Inter:wght@400;500;600&family=JetBrains+Mono&display=swap' }],
  ],
  markdown: {
    math: true,
    languages: ['fortran-free-form', 'fortran-fixed-form'],
    languageAlias: {
      'fortran': 'fortran-free-form',
      'f90': 'fortran-free-form',
      'f95': 'fortran-free-form',
      'f03': 'fortran-free-form',
      'f08': 'fortran-free-form',
      'f77': 'fortran-fixed-form',
    },
  },
  themeConfig: {
    logo: { light: '/logo.svg', dark: '/logo-dark.svg', alt: 'FiNeR' },
    nav: [
      { text: 'Home', link: '/' },
      {
        text: 'Guide',
        items: [
          { text: 'About',             link: '/guide/' },
          { text: 'Features',          link: '/guide/features' },
          { text: 'Installation',      link: '/guide/installation' },
          { text: 'Usage',             link: '/guide/usage' },
          { text: 'API Reference',     link: '/guide/api-reference' },
          { text: 'Contributing',      link: '/guide/contributing' },
          { text: 'Coverage Analysis', link: '/guide/coverage-analysis' },
          { text: 'Changelog',         link: '/guide/changelog' },
        ],
      },
      { text: 'API', link: '/api/' },
      { text: 'GitHub', link: 'https://github.com/szaghi/FiNeR' },
    ],
    sidebar: {
      '/guide/': [
        {
          text: 'Introduction',
          items: [
            { text: 'About',        link: '/guide/' },
            { text: 'Features',     link: '/guide/features' },
          ],
        },
        {
          text: 'Getting Started',
          items: [
            { text: 'Installation',  link: '/guide/installation' },
            { text: 'Usage',         link: '/guide/usage' },
            { text: 'API Reference', link: '/guide/api-reference' },
          ],
        },
        {
          text: 'Project',
          items: [
            { text: 'Contributing',      link: '/guide/contributing' },
            { text: 'Coverage Analysis', link: '/guide/coverage-analysis' },
            { text: 'Changelog',         link: '/guide/changelog' },
          ],
        },
      ],
      '/api/': [
        {
          text: 'API Reference',
          items: [
            { text: 'Overview', link: '/api/' },
          ],
        },
        ...apiSidebar,
      ],
    },
    search: {
      provider: 'local',
    },
  },
  mermaid: {},
  vite: {
    build: {
      target: 'es2022',
    },
    optimizeDeps: {
      include: ['mermaid'],
    },
  },
})
