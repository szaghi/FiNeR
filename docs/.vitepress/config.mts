import { withMermaid } from 'vitepress-plugin-mermaid'
import apiSidebar from '../api/_sidebar.json'

const sidebarGuide = [
  {
    text: 'Start here',
    items: [
      { text: 'Introduction', link: '/guide/' },
      { text: 'Installation', link: '/guide/installation' },
    ],
  },
  {
    text: 'Tutorial',
    items: [
      { text: 'Overview',                      link: '/manual/' },
      { text: '1. A first file',               link: '/manual/tutorial/01-first-file' },
      { text: '2. Reading the case file',      link: '/manual/tutorial/02-reading' },
      { text: '3. Optional settings',          link: '/manual/tutorial/03-defaults' },
      { text: '4. Walking the file',           link: '/manual/tutorial/04-walking' },
      { text: '5. Editing and saving',         link: '/manual/tutorial/05-editing' },
      { text: '6. Writing from scratch',       link: '/manual/tutorial/06-writing' },
      { text: '7. Other INI dialects',         link: '/manual/tutorial/07-dialects' },
      { text: '8. Robust input',               link: '/manual/tutorial/08-robust' },
    ],
  },
  {
    text: 'Recipes',
    items: [
      { text: 'Cookbook', link: '/manual/cookbook' },
    ],
  },
  {
    text: 'Reference',
    items: [
      { text: 'Feature map',      link: '/guide/features' },
      { text: 'The INI syntax',   link: '/guide/syntax' },
      { text: 'Loading, saving, printing', link: '/guide/files' },
      { text: 'Reading values',   link: '/guide/reading' },
      { text: 'Writing values',   link: '/guide/writing' },
      { text: 'Exploring a file', link: '/guide/exploring' },
      { text: 'Errors',           link: '/guide/errors' },
      { text: 'Methods summary',  link: '/guide/api-reference' },
    ],
  },
  {
    text: 'Project',
    items: [
      { text: 'Changelog',         link: '/guide/changelog' },
      { text: 'Contributing',      link: '/guide/contributing' },
      { text: 'Coverage analysis', link: '/guide/coverage-analysis' },
    ],
  },
]

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
      { text: 'Start here', link: '/guide/', activeMatch: '^/guide/(index|installation)?$' },
      { text: 'Tutorial', link: '/manual/tutorial/01-first-file', activeMatch: '^/manual/(index|tutorial/)' },
      { text: 'Cookbook', link: '/manual/cookbook', activeMatch: '^/manual/cookbook' },
      {
        text: 'Reference',
        link: '/guide/features',
        activeMatch: '^/guide/(features|syntax|reading|writing|exploring|files|errors|api-reference)',
      },
      { text: 'API', link: '/api/' },
      {
        text: 'Project',
        items: [
          { text: 'Changelog',         link: '/guide/changelog' },
          { text: 'Contributing',      link: '/guide/contributing' },
          { text: 'Coverage analysis', link: '/guide/coverage-analysis' },
          { text: 'GitHub',            link: 'https://github.com/szaghi/FiNeR' },
        ],
      },
    ],
    sidebar: {
      '/guide/': sidebarGuide,
      '/manual/': sidebarGuide,
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
    socialLinks: [{ icon: 'github', link: 'https://github.com/szaghi/FiNeR' }],
    outline: { level: [2, 3] },
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
