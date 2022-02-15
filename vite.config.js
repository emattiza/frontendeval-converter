import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  root: "public",
  server: { hmr: { clientPort: 443 } },
  plugins: [elmPlugin()],
});
