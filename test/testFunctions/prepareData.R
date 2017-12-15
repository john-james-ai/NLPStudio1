blogs <- readLines(con = './test/testData/hc/en_US.blogs.txt')
news <- readLines(con = './test/testData/hc/en_US.news.txt')
twitter <- readLines(con = './test/testData/hc/en_US.twitter.txt')

blogs <- blogs[1:2000]
news <- news[1:2000]
twitter <- twitter[1:2000]

writeLines(blogs, con = './test/testData/en_US.blogs.txt')
writeLines(news, con = './test/testData/en_US.news.txt')
writeLines(twitter, con = './test/testData/en_US.twitter.txt')
