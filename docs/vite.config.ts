import { defineConfig } from 'vite'
import { fileURLToPath } from 'node:url'
import tailwindcss from '@tailwindcss/vite'

// The PureScript output lives in ../output, so npm packages imported from FFI
// modules (e.g. minisearch) aren't resolvable from the importer's directory.
// Alias them explicitly to this package's node_modules.
const pkg = (name: string) =>
  fileURLToPath(new URL(`./node_modules/${name}`, import.meta.url))

// The published version of the `puppy` npm package, resolved at build time
// and inlined as `__PUPPY_VERSION__` (see src/.../Version.js). The header
// renders it as a version pill. Falls back to an empty string (pill hidden) if
// the registry is unreachable, e.g. offline dev.
async function fetchPuppyVersion(): Promise<string> {
  try {
    const res = await fetch('https://registry.npmjs.org/purs-puppy/latest')
    if (!res.ok) return ''
    const json = (await res.json()) as { version?: string }
    return json.version ?? ''
  } catch {
    return ''
  }
}

export default defineConfig(async () => ({
  define: {
    __PUPPY_VERSION__: JSON.stringify(await fetchPuppyVersion()),
  },
  plugins: [
    tailwindcss(),
  ],
  resolve: {
    alias: {
      minisearch: pkg('minisearch'),
    },
  },
}))
