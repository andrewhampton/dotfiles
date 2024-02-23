if defined? Reline::Face
  Reline::Face.config(:completion_dialog) do |conf|
    conf.define(:default, foreground: "#E5E9F0", background: "#3B4252")
    conf.define(:enhanced, foreground: "#ECEFF4", background: "#4C566A")
    conf.define(:scrollbar, foreground: "#E5E9F0", background: "#4C566A")
  end
end
