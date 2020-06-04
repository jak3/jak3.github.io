# micae

## English

micae is a latin word that means **crumbs**.

It is a collection of phrases, advices, thoughts, aphorisms and pearls of wisdom
that I have caught from lectures, books and online post. micae was born as a
self-reference *e-*notecard system. More about what a notecard system is can be
found in an admirably [post by Ryan Holiday](https://ryanholiday.net/the-notecard-system-the-key-for-remembering-organizing-and-using-everything-you-read/).

Addicted readers must have a look to [his blog](https://ryanholiday.net/).

I have chosen an electronic version for different reasons:

1. It is really fast to find something
1. Can be shared
1. You can keep it with you pretty much everywhere
1. Resist much better to bad weather ;)

## Italiano

micae e' una parola latina che significa **briciole**.

E' una collezione di frasi, consigli, pensieri, aforismi e perle di saggezza che
ho preso da letture, libri e post online. Nata come una versione elettronica del
*notecard system*. Informazioni a proposito possono essere trovate nell'
ammirabile [post di Ryan Holiday](https://ryanholiday.net/the-notecard-system-the-key-for-remembering-organizing-and-using-everything-you-read/).

I lettori accaniti dovrebbero dare un'occhiata al [suo blog](https://ryanholiday.net/).

Ho scelto di adottare una versione elettronica del sistema proposto per diverse
ragioni:

1. E' veramente veloce trovare qualcosa trascritto
1. Può essere condiviso
1. Puoi portare le tue note con te praticamente ovunque
1. Resiste molto meglio alle intemperie ;)

# Dev

`git checkout develop`
`stack exec condenso build && stack exec condenso watch`
`git checkout master`
`
rsync -a --filter='P _site/'      \
         --filter='P _cache/'     \
         --filter='P .git/'       \
         --filter='P .gitignore'  \
         --filter='P .stack-work' \
         --delete-excluded        \
         _site/ .
`
