package com.eucappelli.utils;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

public class PropertiesLoader {

    public static String getBotToken() {
        Properties properties = new Properties();
        try (FileInputStream fis = new FileInputStream("config.properties")) {
            properties.load(fis);
            return properties.getProperty("bot.token");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String getBotUsername() {
        Properties properties = new Properties();
        try (FileInputStream fis = new FileInputStream("config.properties")) {
            properties.load(fis);
            return properties.getProperty("bot.username");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
