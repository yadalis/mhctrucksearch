import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
    plugins: [elmPlugin()],
    //     server: {
    //     hmr: true, // Ensure HMR is enabled
    //   },
})