---
title: How Apple lost its Scientists
date: 2017-02-02 22:00
draft: false
images: true
tags: Apple
---
**Or how Jeremy Clarkson helped me understand why Apple's Macbook Pro was such a disappointment.**

As a machine-learning professional I can't avoid deep-learning, and since [14 of the 16 deep-learning toolkits are NVidia-only](https://en.wikipedia.org/wiki/Comparison_of_deep_learning_software) I need a machine which has an NVidia GPU to do my job. Apple stopped selling such computers back in 2014. Therefore when Apple released its new Macbook Pros with
ATI cards I [joined the chorus of dismay](/2016/11/22/gently-poached-professionals) writing, among many other things

> What's astonishing is Apple built a pro computer completely around GPUs, the Mac Pro, but chose an ATI GPU. Did they not talk to any end-users?

Recently, Jeremy Clarkson helped me realise where Apple's gone wrong.

First lets consider Apple's reasoning. They correctly anticipated the need for scientific computing on the GPU, but 80% of the computers they sell are iPhones and iPads, for which NVidia sells no suitable chipset. Therefore they couldn't use CUDA, NVidia's proprietary maths library[^gpu-math-stack].

So instead they decided to promote a cross-platform API: OpenCL. NVidia was already number one, so they asked the number two -- AMD/ATI -- to be a partner.

ATI GPUs have the further advantage that they have much [less power-draw than NVidia GPUs](https://9to5mac.com/2016/11/16/macbook-pro-why-amd-gpu/), which made them a great choice for graphics cards in consumer laptops.

For a prosumer laptop, one can get a lot more OpenCL power by just adding a faster ATI card. And one can make a minimalist desktop like the iMac by reusing a lot of laptop components.

At this stage you're using ATI the whole way up, so there are significant efficiencies in scale in just standardising across the line, and putting ATI GPUs in the only remaining computer, your professional computer for scientific users, the Mac Pro.

The only problem is you can no more do scientific computing on the Mac Pro than you can write computer games on a machine unsupported by either the Unreal or Unity game engines.

And this is where Jeremy Clarkson comes in.

In the [Censored to Censored](https://www.amazon.co.uk/censored-to/dp/B01J93ZV7A) episode of the [Grand Tour](https://www.amazon.co.uk/The-Grand-Tour-Season-1/dp/B01J93ZNN2), Hammond, May and Clarkson reviewed three SUVs, the [Janguar F-Pace](http://www.jaguar.co.uk/jaguar-range/f-pace/index.html), the [Bentley Bentayga](http://www.bentleymotors.com/en/models/bentayga/bentayga.html) and a [Range Rover](http://www.landrover.co.uk/vehicles/range-rover/index.html). It all ended with a race around the track, which Clarkson won by cheating: he simply left the dirt road and went cross country.

<div align="center">
<img src="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" data-src="/images/gt-rr-01-general-driving-retina.jpg" style="width: 80%; height:auto" alt="Finally, it was the turn of the best car here. However I had no intention of relying on my supreme driving skills."/>

<img src="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" data-src="/images/gt-rr-02-interior-retina.jpg" style="width: 80%; height:auto" alt="You see the thing is, the Jaguar and the Bentley were designed as road cars and then given some off-road ability"/>

<img src="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" data-src="/images/gt-rr-03-uphill-retina.jpg" style="width: 80%; height:auto" alt="Whereas the Range Rover was designed as an off-road car, and then given some ability to work on the road"/>

<img src="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" data-src="/images/gt-rr-04-display-retina.jpg" style="width: 80%; height:auto" alt="This car senses what sort of terrain its driving over and then engages or disengages the differentials accordingly."/>

<img src="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" data-src="/images/gt-rr-05-skating-uphill-retina.jpg" style="width: 80%; height:auto" alt="You could not come up here in the Bentley or the Jaguar. Look at that, look at it! What a machine you are!"/>
</div>


This epitomises the difference between the prosumer and the professional. The Bentley and Jaguar are big enough and burly enough to work better on dirt tracks than a standard car, but if you take them off-road, they'll get stuck instantly. For people whose jobs require off-road capability, only the Range Rover makes sense, because only it has the odd, peculiar, things a road-car would never have: things such as adjustable suspension; front & rear electronic locking differentials; or extremely low gearings.

Great professional hardware -- motoring or computing -- is created when one starts from needs and work back to a chassis. You will never succeed starting with a chassis and trying to scale up. Form has to follow function, not dictate it.

Yet this is exactly what Apple's been doing the last five years. Final Cut X had a great new UI, and was an improvement on iMovie, but on release lacked [the peculiar features professional video editors need](http://www.premiumbeat.com/blog/final-cut-pro-x-the-missing-features/). Photos.app is better than iPhoto, but it lacks all the editing and curation features Lightroom has. The Macbook Pro is faster than a Macbook, but lacks a sufficiently powerful NVidia GPU necessary to do scientific computing or game-development. The iMacs and Mac Pros are faster than Macbook Pros, but feature the same hardware trade-offs, and so are similarly disqualified from many professions.

Apple did once ship great professional tools. The 2010 15" Macbook Pros had an NVidia graphics card and a Unix OS, but Apple gave it the easy ergonomics of a consumer laptop by using a separate embedded GPU whenever possible to save battery, and providing a macOS shell to make Unix easy. That was a great _professional_ laptop.

In the last five years, Apple has moved away from this philosophy, selling F-Paces instead of Range Rovers. For a while it was safe to do so, since the PC industry was still selling the computing equivalent of [Land Rover Defenders](http://www.landrover.co.uk/vehicles/defender/index.html): bulky ungainly things that were tolerable at work and a chore at home.

Unfortunately for Apple, Microsoft and Dell are now selling the computing equivalents of Range Rovers in their Surface and XPS ranges, and Apple is in the invidious position where it must sell dongles at a discount in order to lure professionals into purchasing its prosumer PCs. I suspect many won't: in my case I'll need a new computer this year, and Apple isn't selling anything I can use.



[^gpu-math-stack]: If you're unfamiliar with the way math libraries are structured, here's an analogy to graphics. At the top end you have math and deep-learning toolkits like [Theano](http://deeplearning.net/software/theano/) and [Caffe](http://caffe.berkeleyvision.org/) which are like game engines such as Unreal or Unity. These are built on [BLAS](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) and [Lapack](https://en.wikipedia.org/wiki/LAPACK) libraries, which are like [OpenGL](https://en.wikipedia.org/wiki/OpenGL) and [GLUT](https://en.wikipedia.org/wiki/OpenGL_Utility_Toolkit). CPU vendors often release libraries following the BLAS/Lapack API to expose their chips' features (e.g. the [Intel MKL](https://software.intel.com/en-us/intel-mkl)). Once one starts doing maths on the GPU, there is one additional layer: [NVidia CUDA](https://en.wikipedia.org/wiki/CUDA) or
[OpenCL](https://en.wikipedia.org/wiki/OpenCL), which are analoguous to [Metal](https://developer.apple.com/metal/) or [Vulkan](https://en.wikipedia.org/wiki/Vulkan_%40API%41).
