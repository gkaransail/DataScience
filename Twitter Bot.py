import tweepy
import Keys_test


def api():
    auth = tweepy.OAuthHandler(Keys_test.api_key, Keys_test.api_secret)
    auth.set_access_token(Keys_test.access_token, Keys_test.access_token_secret)
    return tweepy.API(auth)



def tweet(api: tweepy.API, message:str, image_path=None):
    if image_path:
        api.update_status_with_media(message, image_path)
    else:
        api.update_status(message)

    print('Tweeted successfully!')



if __name__ == '__main__':
    api = api()
    tweet(api, "This was tweeted from Python")