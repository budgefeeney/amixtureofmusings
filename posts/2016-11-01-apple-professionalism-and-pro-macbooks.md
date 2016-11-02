---
title: Apple, Professionalism, and Pro Macbooks
date: 2016-11-01 17:00
draft: false
tags: Apple, MachineLearning
---

The [Macbook "One"](http://www.apple.com/macbook/) is a great computer. For the cost of having a dongle at home, you get a computer that can fit in your handbag or satchel, do every casual computing task you need, and which almost never needs to be plugged in. I don't even mind the need for dongles: it's no different to the original iMac's lack of a floppy drive[[1]](#fn-usb-thumb) or the original Macbook Air's lack of ports and a CD drive[[2]](#fn-air-youtube). It's the ideal computer for the casual user.

Professional users however require computers that allow them do their job. This doesn't mean more powerful overall, it means having more power in the right places. Battery life and thinness aren't as important: professionals work in offices, not coffee shops.

Unfortunately, it's become clear in recent years that -- software developers aside -- Apple has no idea what its professional customers want, a failing epitomised by the new [Macbook Pro](https://www.apple.com/macbook-pro/).

Consider the Mac Pro. Apple correctly inferred that maths on the GPU would be important, and built a computer around a pair of ATI GPUs. The problem is NVidia was the first to pioneer maths on the GPU, and has spent much more time and money improving it than ATI. The result is that all the major GPU math and deep-learning toolkits -- [Theano](http://deeplearning.net/software/theano/), [ArrayFire](http://arrayfire.com),  [DL4J](https://deeplearning4j.org), [Caffe](http://caffe.berkeleyvision.org), [Torch](http://torch.ch) -- work exclusively on NVidia CUDA, with ArrayFire being the lone exception.

So you can't do deep-learning research on a Mac Pro[[3]](#fn-from-scratch).

In fact it's even worse than that. Over the last four years Apple has transitioned to using ATI cards exclusively across its entire range. Consequently, as a professional machine-learning researcher[[4]](#fn-career), Apple no longer sells *any* computer that satisfies my professional needs.

With particular regard to laptops, apologists waffle about NVidia's battery-draw, but the [Razer Blade](http://www.razerzone.com/gaming-systems/razer-blade) gets about 6 hours battery, with an NVidia card in an enclosure that resembles a late 2011 Macbook Pro, which is acceptable to most. The [Surface Book](https://www.microsoft.com/en-us/surface/devices/surface-book) also features an NVidia card and advertises 16 hours of battery life. The 15" Dell XPS with an old NVidia 960M GPU has [5 hours](https://www.engadget.com/2016/04/06/dell-xps-15-review/). Note that the Blade and XPS have 4K displays unlike the Macbook Pro's low-fi Retina screen, and it is their displays rather than their discrete GPUs that account for much of the power-draw.

Judging by the press, mine is not the only profession being left behind. Apple sells nothing as useful to artists as the new [Microsoft Surface Studio](https://www.microsoft.com/en-us/surface/devices/surface-studio). The price seems staggering until [as Mike Krahulik of Penny Arcade pointed out](https://www.penny-arcade.com/news/post/2016/10/26/the-surface-studio), you realise that the cumulative cost of a [Cintiq](http://www.wacom.com/en-us/products/pen-displays) and [Retina iMac](https://www.apple.com/imac) is actually higher. Apple's solution is to just use an iPad Pro, but there are almost no Pro apps on the iPad Pro -- [no Photoshop, no Illustrator no Indesign](http://www.digitalartsonline.co.uk/news/creative-hardware/why-apple-ipad-pro-isnt-right-for-pro-artists-designers-yet/) -- and this is entirely Apple's fault for failing to curate and develop a market for complex applications. The iPad apps that do exist are "sketch" apps like Adobe Comp CC, which are of little practical use, and don't appear to be earning much money.

Then there's [Microsoft's Dial](http://www.wacom.com/en-us/products/pen-displays). Today's Apple also would never, I'm convinced, have designed something as intuitive as the dial for desktop PCs. Apple's peripherals have always been worse than Microsoft's: from the puck mouse, to the painful headphones to its new unpleasant ultra-thin keyboards, Apple has consistently put how peripherals look over how they work.

Artists are in good company. Professional photographers who bet their business on Apple's software got burnt when Apple abandoned Aperture. They've since moved to Lightroom, and for them the new laptops are only slightly worse, in that they require an SD Card dongle.

Filmographers have had a similar experience: while Microsoft clearly spoke to artists before releasing the Surface Studio, Apple evidently didn't speak, or didn't listen to, filmographers before realising Final Cut X. They've not been well taken-care-of by Apple's software teams.

Coming back to my own needs, my mind boggles at the thought of the scientific computing labs that went with Mac Pros. Three years in they have no upgrade path, and two-year-old PCs with NVidia Maxwell cards offer dramatically more compute power, regardless of price. I would argue that there really is a significant business-risk in buying Mac Pros these days.

In that sense the Mac Pro update failed horribly: intended to announce Apple's continued commitment to professional computing, it is now grim monument to Apple's idle-minded disregard for its pro users. The [product page itself](http://www.apple.com/mac-pro/performance/) is a sad farce, with its proud declaration of how well the Mac Pro supports Aperture 3, the pro software app Apple casually discontinued two years ago.

I, as a machine-learning researcher, cannot justify buying a Macbook Pro, indeed any Mac, as a professional machine. Artists live in a world where an iMac is clearly the second best choice to a Surface Studio. Photographers and filmographers have learned not to trust that Apple will support its software, and many I suspect are now in a situation where their software runs on Windows and Mac, and so the switching-cost is low.

We're all niche users of course, the overwhelming majority of Macbook Pros are sold neither to scientists nor creatives; rather they go to casual users and generalist programmers[[5]](#fn-programmers-and-the-pro). Yet it was perfectly possible for Apple to make a professional laptop. The Macbook trades power for portability: a better Macbook Pro would have distinguished itself from its sibling by trading portability for power, power that was usable by *professional* users.

Instead the new Macbook Pro is a scaled up Macbook with [a medicore performance increase](http://arstechnica.com/gadgets/2016/10/amd-radeon-pro-400-series-specs-macbook-pro/) in all the wrong areas.


-----
 1. <a name="fn-usb-thumb" /> Launched a couple of years before USB flash-drives and broadband became common.
 2. <a name="fn-air-youtube" />A computer whose first incarnation could only play Youtube videos for three or four minutes before overheating
 3. <a name="fn-from-scratch" />OpenCL forks and plugins exist for some of these libraries, but they're not reliable and often incomplete. The other approach of course is to write ones own library from scratch, but why spend $2999 on a machine that *increases* the amount of work one has to do.
 4. <a name="fn-career" />Albeit one who's finishing a four-year break where he did a PhD
 5. <a name="fn-programmers-and-the-pro" />Though even some programmers are unhappy with the new Macbook Pro, as they believe they need 32GB to do proper development. I confess I have to wonder what sort of app, and what class of implementation, needs more than 10GB to debug or compile.


 Figure double-retweet ID issue, affects URL removal
