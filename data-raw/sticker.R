library(hexSticker)
library(councilR)


showtext::showtext_auto()
sysfonts::font_paths()
files <- sysfonts::font_files()
sysfonts::font_add("HelveticaNeueLT Std Cn", "HelveticaNeueLTStd-Cn.otf")
sysfonts::font_add("HelveticaNeueLT Std Lt", "HelveticaNeueLTStd-Lt.otf")
sysfonts::font_add("HelveticaNeueLT Std Thin", "HelveticaNeueLTStd-Th.otf")
sysfonts::font_add("Arial Narrow", "Arial Narrow.ttf")
sysfonts::font_add("HelveticaNeueLT Std Med Cn", "HelveticaNeueLTStd-MdCn.otf")
sysfonts::font_add("Palatino Linotype", "pala.ttf")


print(
  sticker(
    subplot = "data-raw/Metropolitan Council Mark White.png",
    package = "councilR",
    p_size = 27,
    p_color = "white",
    h_fill = councilR::colors$councilBlue,
    s_x = 1,
    s_y = 0.85,
    s_height = 0.45,
    s_width = 0.45 * 1.1,
    white_around_sticker = FALSE,
    h_size = 3,
    h_color = councilR::blue_cascade$level1$background,
    p_x = 1,
    p_y = 1.45,
    p_family = "HelveticaNeueLT Std Thin",
    url = "github.com/Metropolitan-Council/councilR",
    u_color = "gray85",
    u_family = "Palatino Linotype",
    u_size = 3.5,
    dpi = 300
  )
)

# saved manually at 518 x 600
