---
title: Poached Frogs and Professional Mac Users
date: 2016-11-22 18:00
draft: false
tags: Apple
---
The old adage says that if you drop a frog in boiling water he'll jump out, but if you put him in cold water and slowly heat it up, he'll happily sit there till he's poached to fatal perfection.

It's an awful metaphor. Our English forebears had some grim imaginations.[^eatinghabits]

[^eatinghabits]:And quite possibly some terrible eating habits.

Anyway, if one were to ask the frog if he was happy with his new situation, he'd probably say no. Why? Well, the frog would notice it was a bit warmer, but it had been getting warm for a while, so he'd discount that. What he would see was that suddenly there were lot of bubbles around, and -- looking for some tangible detail -- the frog would decide it was the bubbles that were to blame for his discomfort.

This is the problem of criticism. People know when they don't like something, but they often have trouble articulating the root cause of their dislike. Instead they latch onto the most obvious, tangible difference. Film Critic Hulk[^fch] has [discussed this in the past](https://filmcrithulk.wordpress.com/2011/06/07/hulk-essay-your-ass-tangible-details-and-the-nature-of-criticism/) in the context of movie reviews, where people may focus on a tangible detail, such as the silly emo-Tobey-Macguire scene in Spiderman 3, instead of the broader issue, which in that case was the fact that the movie didn't find a consistent tone in which such a scene could work.

[^fch]: Ostensibly the Incredible Hulk writing film reviews, though they occasional break character to suggest they're a professional screen-writer in Hollywood pretending to be the Incredible Hulk writing film reviews. Then again they may well be a randomer in a basement pretending to be a screen-writer pretending to be the Hulk writing movie reviews. The point is, they write good movie reviews.

Something similar is happening with the Macbook Pro and its critics. They're blinded by bubbles -- ports and RAM -- and haven't taken the long view necessary to see the true cause of their unease.

## Gently Poached Professionals

Things have been getting slowly, inexorably, and continuously worse for Apple's professional customers over the last five years.

In 2005 Apple suggested professional photographers should use its new app, Aperture. In 2014 Apple discontinued it. Out of the upgrade cycle, users had to pay full whack for Lightroom and re-train.

Final Cut Pro, another app from Apple, stagnated for several years in the noughties, with few updates and no 64-bit support. So it was a great relief when Apple released Final Cut X in 2011. Professional's relief turned to ashes when they realised [Apple had dropped all the awkward pernickety little features necessary to get actual work done](http://www.premiumbeat.com/blog/final-cut-pro-x-the-missing-features/). Apple promised plugins would address this, but as they had not forewarned developers, plugins were slow to arrive. During this time Final Cut's users struggled in a way Adobe After Effect's didn't.

Apple has abandoned its scientific computing users. NVidia has massively invested in maths on the GPU -- both in software and hardware -- while ATI has bleated about open standards and spent a pittance. The result is that of the [16 deep-learning toolkits 14 support NVidia while just two support ATI](https://en.wikipedia.org/wiki/Comparison_of_deep_learning_software)[^plugins]. A similar situation exists with general-purpose linear-algebra toolkits. Apple stopped selling computers, of any kind, with NVidia cards in 2014.

[^plugins]: Of course plugins and forks exist to provide partial OpenCL support for some of these, but you would be foolish to bet your professional career on such unsupported software.

This particularly affects me, as I do machine-learning research. There is no machine that Apple sells that I can justify buying for professional use.

What's astonishing is Apple built a pro computer completely around GPUs, the Mac Pro, but chose an ATI GPU. Did they not talk to any end-users?

Indeed the worst affected are corporate buyers of Mac Pros, which has seen no update in three years. Not only does it have [frequently faulty GPUs](http://www.kitguru.net/lifestyle/apple/matthew-wilson/apple-launches-repair-program-for-mac-pros-after-gpu-failures/) and no upgradability; but there's no hope of any new model in the future, and even if a new model were to arrive, there's no second-hand market for such old hardware. Like Aperture users, Mac Pro owners have to write-off the entire cost and buy something completely new, out of the upgrade cycle[^mac-dismal].

[^mac-dismal]:In a dismal irony, the Mac Pro, championed as the computer that would reaffirm Apple's commitment to its professionals, has instead become a monument to their neglect. Not only has Apple not updated the computer, they even forgot to update its [product webpage](https://web.archive.org/web/20161019071505/https://www.apple.com/mac-pro/performance/) which until the furore this month, had embarrassingly touted how well the the Mac Pro ran Aperture.

This was the background to the Apple event on October 27th, and to the response that followed.

## Prosumers and Professionals

Just before Apple's event, Microsoft had one of its own. They launched two computers -- a laptop and a desktop -- with touch-screens and styli. The laptop's screen could be detached to form a tablet; the desktop -- which looked like an iMac -- could be arranged into an easel. This desktop optionally came with was a touch-sensitive dial for fine-grained adjustments. Thanks to their work with Adobe, Microsoft could use Photoshop and Illustrator to demonstrate how well these new computers worked.

These were genuinely novel.[^snarky-gruber]

[^snarky-gruber]: Apologists [snarkily griped](http://daringfireball.net/linked/2016/10/28/panzarino-touch-bar) that the stylus wasn't as good as the Pencil on an iPad Pro, but there is no version of Adobe Photoshop and Illustrator for the iPad Pro. In fact due to Apple's curation of the iOS store [there are very few people developing pro apps for iOS at all](http://www.cheatsheet.com/gear-style/apples-new-ipad-pro-why-app-developers-are-not-too-happy.html/?a=viewall).

By contrast Apple announced a slightly faster, slightly smaller laptop. It had largely the same form factor, but with reduced functionality to support its smaller size. It also had a touchbar as a new input-method, and it was very evident that in execution, and even ambition, it was more limited than the dial.

In the thinking that saw both Pixelmator and Photoshop take equal billing in the launch, in Final Cut X's absent features, in the switch to ATI and the [rationale for the 16GB limit on the Macbook Pro](http://daringfireball.net/linked/2016/10/31/intel-mbp-ram), we find the root cause of Apple's professional malaise

Apple has conflated _professionals_ -- whose needs are awkward and particular -- with _prosumers_ who just want a little more of everything. This has coincided, and may be a result of, a general disinterest in developing professional software and hardware.

Consider the GPU for instance. ATI GPUs have a [far lower power draw](https://9to5mac.com/2016/11/16/macbook-pro-why-amd-gpu/) than NVidia GPUs. Since casual users value portability, an ATI GPU makes sense. For professionals, only an NVidia GPU is good enough for scientific computing, VR, or game development.

Thus a professional computer should ship with NVidia GPUs irrespective of the adverse effect on battery life and portability. Portability is a secondary concern: professionals work in offices, not coffee shops.

This too is why many professionals would be happy if the most expensive Macbook Pro sacrificed battery-life to raise the RAM ceiling.

Fundamentally, if you try to scale up a machine aimed at casual users, you'll miss the things professionals _need_ to get work done, since professional needs are often slightly esoteric. Instead one should start with what professionals need, then work down to a chassis.

Modern Apple doesn't seem to know what professionals need. [Microsoft invited several artists to discuss the Surface Studio during its design](https://www.penny-arcade.com/news/post/2016/10/26/the-surface-studio), film-makers were excluded from the development of Final Cut X.

What's worse is the laptop-isation extends to the entire line. Having chosen ATI as the best supplier of GPUs for portables, Apple standardised on ATI as a supplier, and now includes mediocre ATI GPUs in desktops and the Mac Pro. These computers consequently fail to meet the needs of gamers, VR developers, or scientific-computing professionals.


## Why Bother at All?

As things stand, there is a significant business risk in choosing to purchase professional software or hardware from Apple rather than from Adobe or PC makers. The best possible computer that one can use for Photoshop or Illustrator is a Surface Studio. Apple sells no computer suitable for professionals in machine-learning or scientific computing.

But so what? Macs make up only [12% of Apple's revenue](http://www.macworld.co.uk/news/apple/apple-q4-2016-financial-results-iphone-mac-sales-down-again-3581769/), and creative professionals and scientists account for a small minority of that 12% in turn.

The reason is Beats Audio.

Apple, you'll recall, paid $5bn for a maker of mediocre head-phones with a second-best streaming service. I couldn't understand it at first, but in the end I could come up with only one reason: the importance of the humanities to Apple's brand.

The iPod, and iTunes store, completely changed Apple. What once was a niche PC maker, suddenly became a maker of fashionable, electronic life-style accessories. This required that certain sense of cool that a love of music can provide.

I think Apple bought beats to reaffirm its commitment to music, and through it, fashion and lifestyle.

The public, conspicuous use of Apple hardware by professionals in art, publishing, music and science similarly adds to Apple's brand. In particular, it's an affirmation of Apple's reputation for creating best-of-class hardware, and of that hardware's potential to engender successful professional careers.

Apple is now at risk of losing these users, and by extension, damaging their brand. By upscaling casual computers for prosumers, and blithely forgetting to cater to the particular needs of its professional customers, Apple risks losing its professionals altogether.

If that were to happen, Apple would return to where it was in the nineties: a purveyor of undeniably fashionable computers for people who only need to pretend to work.
