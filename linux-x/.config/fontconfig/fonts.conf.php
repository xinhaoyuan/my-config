<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>

  <!--  <dir>~/.fonts/artwiz-a:unscaled</dir>
       <dir>~/.fonts/non_zh</dir>
       <dir>~/.fonts/VeraSansYuanTi</dir> !-->

  <!-- 最小字号 !-->
  <match target="pattern" >
    <test compare="less_eq" name="pixelsize" >
      <double>10</double>
    </test>
    <edit mode="assign" name="pixelsize" >
      <double>10</double>
    </edit>
  </match>

  <!-- 别名替换 !-->
  <match target="pattern" >
    <test name="family" >
      <string>宋体</string>
    </test>
    <edit mode="assign" name="family" >
      <string>SimSun</string>
    </edit>
  </match>
  <match target="pattern" >
    <test name="family" >
      <string>新宋体</string>
    </test>
    <edit mode="assign" name="family" >
      <string>NSimSun</string>
    </edit>
  </match>

  <alias>
    <family>sans-serif</family>
    <prefer>
      <family>DejaVu Sans</family>
      <!-- <family>AR PL UMing CN</family> -->
      <!-- <family>AR PL UMing HK</family> -->
      <family>Vera Sans YuanTi</family>
      <family>Microsoft YaHei</family>
      <family>SimSun</family>
    </prefer>
  </alias>

  <!-- For strange behavior that fontconfig ignore the prefer order -->
  <match>
    <test name="family">
      <string>sans-serif</string>
    </test>
    <edit name="family" mode="prepend" binding="strong">
      <string>DejaVu Sans</string>
    </edit>
  </match>

  <alias>
    <family>serif</family>
    <prefer>
      <family>DejaVu Serif</family>
      <!-- <family>AR PL UMing CN</family> -->
      <!-- <family>AR PL UMing HK</family> -->
      <family>SimSun</family>
    </prefer>
  </alias>

  <!-- For strange behavior that fontconfig ignore the prefer order -->
  <match>
    <test name="family">
      <string>serif</string>
    </test>
    <edit name="family" mode="prepend" binding="strong">
      <string>DejaVu Serif</string>
    </edit>
  </match>

  <alias>
    <family>monospace</family>
    <prefer>
      <family>DejaVu Sans Mono</family>
      <!-- <family>AR PL UMing CN</family> -->
      <!-- <family>AR PL UMing HK</family>       -->
      <family>Vera Sans YuanTi Mono</family>
      <family>SimSun</family>
    </prefer>
  </alias>

  <!-- 双宽度修正 !-->

  <match target="font" >
    <test compare="contains" target="pattern" name="lang" >
      <string>zh</string>
    </test>
    <test compare="eq" name="spacing" >
      <const>dual</const>
    </test>
    <edit mode="assign" name="spacing" >
      <const>proportional</const>
    </edit>
    <edit mode="assign" name="globaladvance" >
      <bool>false</bool>
    </edit>
  </match>

  <match target="font" >
    <test compare="contains" target="pattern" name="lang" >
      <string>ja</string>
    </test>
    <test compare="eq" name="spacing" >
      <const>dual</const>
    </test>
    <edit mode="assign" name="spacing" >
      <const>proportional</const>
    </edit>
    <edit mode="assign" name="globaladvance" >
      <bool>false</bool>
    </edit>
  </match>

  <match target="font" >
    <test compare="contains" target="pattern" name="lang" >
      <string>ko</string>
    </test>
    <test compare="eq" name="spacing" >
      <const>dual</const>
    </test>
    <edit mode="assign" name="spacing" >
      <const>proportional</const>
    </edit>
    <edit mode="assign" name="globaladvance" >
      <bool>false</bool>
    </edit>
  </match>

  <!-- UniSun, SimSun 和 MingLiu 的微调 !-->

  <?php foreach ([ "UniSun", "SimSun", "NSimSun", "PMingLiu", "MingLiU" ] as $font_name) { ?>
  <match target="font" >
    <test compare="eq" name="family" qual="any" >
      <string><?=$font_name?></string>
    </test>
    <edit mode="assign" name="rgba" >
      <const>none</const>
    </edit>
    <edit mode="assign" name="antialias" >
      <bool>true</bool>
    </edit>
    <edit mode="assign" name="autohint" >
      <bool>false</bool>
    </edit>
    <edit mode="assign" name="hinting" >
      <bool>true</bool>
    </edit>
    <edit mode="assign" name="hintstyle" >
      <const>hintfull</const>
    </edit>
    <edit mode="assign" name="embeddedbitmap" >
      <bool>true</bool>
    </edit>
  </match>
  <?php } ?>

  <!-- SimSun 的点阵字体 !-->

  <?php foreach ([ "SimSun", "NSimSun" ] as $font_name) { ?>
  <match target="font" >
    <test compare="eq" name="family" qual="any" >
      <string><?=$font_name?></string>
    </test>
    <test compare="more_eq" name="pixelsize" >
      <double>12</double>
    </test>
    <test compare="less_eq" name="pixelsize" >
      <double>18</double>
    </test>
    <test compare="not_eq" name="pixelsize" >
      <double>17</double>
    </test>
    <edit mode="assign" name="antialias" >
      <bool>false</bool>
    </edit>
  </match>
  <?php } ?>

  <!-- UniSun 的点阵字体 !-->
  <match target="font" >
    <test compare="eq" name="family" qual="any" >
      <string>UniSun</string>
    </test>
    <test compare="more_eq" name="pixelsize" >
      <double>10</double>
    </test>
    <test compare="less_eq" name="pixelsize" >
      <double>18</double>
    </test>
    <edit mode="assign" name="antialias" >
      <bool>false</bool>
    </edit>
  </match>

  <!-- Monaco -->
  <match target="font" >
    <test compare="eq" name="family" qual="any" >
      <string>Monaco</string>
    </test>
    <edit mode="assign" name="rgba" >
      <const>rgb</const>
    </edit>
    <edit mode="assign" name="autohint" >
      <bool>false</bool>
    </edit>
    <edit mode="assign" name="antialias" >
      <bool>true</bool>
    </edit>
    <edit mode="assign" name="hinting" >
      <bool>true</bool>
    </edit>
    <edit mode="assign" name="hintstyle" >
      <const>hintslight</const>
    </edit>
  </match>

  <!-- CT Fonts  -->

  <?php foreach ([ "Consolas", "Inconsolata", "Corbel", "Cambria", "Cambria Math", "Candara", "Constantia", "Microsoft YaHei" ]
    as $font_name) { ?>
  <match target="font" >
    <test compare="eq" name="family" >
      <string><?=$font_name?></string>
    </test>
    <edit mode="assign" name="rgba" >
      <const>rgb</const>
    </edit>
    <edit mode="assign" name="autohint" >
      <bool>true</bool>
    </edit>
    <edit mode="assign" name="antialias" >
      <bool>true</bool>
    </edit>
    <edit mode="assign" name="hinting" >
      <bool>true</bool>
    </edit>
    <edit mode="assign" name="hintstyle" >
      <const>hintslight</const>
    </edit>
  </match>
  <?php } ?>

  <!-- wqy 字体的设置 -->

  <?php foreach ([ "WenQuanYi Zen Hei", "文泉驿正黑", "文泉驛正黑" ]
    as $font_name) { ?>

  <match target="font">
    <test qual="any" name="family">
      <string><?=$font_name?></string>
    </test>
    <edit name="globaladvance"><bool>false</bool></edit>
    <edit name="spacing"><int>0</int></edit>
    <edit name="antialias" mode="assign"><bool>true</bool></edit>
    <edit name="hinting" mode="assign"><bool>true</bool></edit>
    <edit name="hintstyle" mode="assign"><const>hintmedium</const></edit>
    <edit name="autohint" mode="assign"><bool>false</bool></edit>
    <edit name="rh_prefer_bitmaps" mode="assign"><bool>false</bool></edit>
    <edit name="rgba" mode="assign"><const>none</const></edit>
    <edit name="embeddedbitmap"><bool>false</bool></edit>
  </match>
  <match target="font">
    <test qual="any" name="family">
      <string><?=$font_name?></string>
    </test>
    <test compare="more_eq" name="pixelsize"><double>11</double></test>
    <test compare="less" name="pixelsize"><double>16</double></test>
    <edit name="antialias" mode="assign"><bool>false</bool></edit>
    <edit name="embeddedbitmap" mode="assign"><bool>true</bool></edit>
    <edit name="hinting" mode="assign"><bool>false</bool></edit>
  </match>
  <?php } ?>


  <!-- 字体替换 -->

  <?php foreach (["Vera Sans YuanTi", "Vera Sans YuanTi Mono", "SimSun", "NSimSun"]
    as $name) { ?>
  <match target="pattern">
    <test name="family" qual="any">
      <string><?=$name?></string>
    </test>
    <test name="pixelsize" compare="less">
      <double>12</double>
    </test>
    <edit mode="assign" name="family">
      <string>UniSun</string>
    </edit>
  </match>
  <?php } ?>

  <!-- From YuanTi -->

  <!-- Win XP 效果 -->
  <match target="font">
    <test name="family"><string>Vera Sans YuanTi</string></test>
    <edit name="globaladvance"><bool>false</bool></edit>
  </match>

  <match target="font">
    <test name="family"><string>Vera Sans YuanTi Mono</string></test>
    <edit name="globaladvance"><bool>false</bool></edit>
  </match>

  <match target="font">
    <test qual="any" name="family"><string>Vera Sans YuanTi</string></test>
    <edit name="antialias" mode="assign"><bool>true</bool></edit>
    <edit name="hinting" mode="assign"><bool>true</bool></edit>
    <edit name="autohint" mode="assign"><bool>false</bool></edit>
    <edit name="hintstyle" mode="assign"><const>hintfull</const></edit>
    <edit name="rh_prefer_bitmaps" mode="assign"><bool>false</bool></edit>
  </match>

  <match target="font">
    <test qual="any" name="family"><string>Vera Sans YuanTi</string></test>
    <test compare="more_eq" name="pixelsize" qual="any" ><double>12</double></test>
    <test compare="less_eq" name="pixelsize" qual="any" ><double>16</double></test>
    <edit name="antialias" mode="assign"><bool>false</bool></edit>
    <edit name="hinting" mode="assign"><bool>true</bool></edit>
    <edit name="autohint" mode="assign"><bool>false</bool></edit>
    <edit name="hintstyle" mode="assign"><const>hintfull</const></edit>
    <edit name="rh_prefer_bitmaps" mode="assign"><bool>true</bool></edit>
  </match>

  <match target="font">
    <test qual="any" name="family"><string>Vera Sans YuanTi Mono</string></test>
    <edit name="antialias" mode="assign"><bool>true</bool></edit>
    <edit name="hinting" mode="assign"><bool>true</bool></edit>
    <edit name="autohint" mode="assign"><bool>false</bool></edit>
    <edit name="hintstyle" mode="assign"><const>hintfull</const></edit>
    <edit name="rh_prefer_bitmaps" mode="assign"><bool>false</bool></edit>
  </match>

  <match target="font">
    <test qual="any" name="family"><string>Vera Sans YuanTi Mono</string></test>
    <test compare="more_eq" name="pixelsize" qual="any" ><double>12</double></test>
    <test compare="less_eq" name="pixelsize" qual="any" ><double>16</double></test>
    <edit name="antialias" mode="assign"><bool>false</bool></edit>
    <edit name="hinting" mode="assign"><bool>true</bool></edit>
    <edit name="autohint" mode="assign"><bool>false</bool></edit>
    <edit name="hintstyle" mode="assign"><const>hintslight</const></edit>
    <edit name="rh_prefer_bitmaps" mode="assign"><bool>true</bool></edit>
  </match>

</fontconfig>
