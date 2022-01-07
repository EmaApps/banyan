module.exports = {
  content: [
    "./src/**/*.hs",
    // TODO: don't hardcode this. To be used in 'gen' stage.
    "./content/.ci/**/*.html"
  ],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('@tailwindcss/forms'),
    require('@tailwindcss/line-clamp'),
    require('@tailwindcss/aspect-ratio')
  ],
}
