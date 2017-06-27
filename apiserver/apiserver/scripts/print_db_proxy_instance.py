from .. import config

if __name__ == "__main__":
    print("{}:{}:{}".format(config.DATABASE_PROJECT_ID,
                            config.DATABASE_REGION,
                            config.DATABASE_INSTANCE_NAME))