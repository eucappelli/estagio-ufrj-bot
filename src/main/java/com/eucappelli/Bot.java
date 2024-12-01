package com.eucappelli;

import com.eucappelli.models.FileDTO;
import com.eucappelli.models.UserDTO;
import com.eucappelli.utils.PropertiesLoader;
import com.eucappelli.utils.Strings;
import com.eucappelli.utils.Validators;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.telegram.telegrambots.bots.TelegramLongPollingBot;
import org.telegram.telegrambots.meta.api.methods.AnswerCallbackQuery;
import org.telegram.telegrambots.meta.api.methods.GetFile;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.methods.updatingmessages.EditMessageReplyMarkup;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.InlineKeyboardButton;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public class Bot extends TelegramLongPollingBot {
    private final List<UserDTO> connectedUsers = new ArrayList<>();
    @Override
    public String getBotUsername() {
        return PropertiesLoader.getBotUsername();
    }

    @Override
    public String getBotToken() {
        return PropertiesLoader.getBotToken();
    }

    @Override
    public void onUpdateReceived(Update update) {
        try {
            log.info("Connected users: {}", connectedUsers);
            if (update.getMessage() == null) {
                handleCallback(update);
            } else {
                handleMessage(update);
            }
        } catch (Exception e) {
            log.error(Strings.ERROR_WHILE_RECEIVING_UPDATE, e);
        }
    }

    private void handleCallback(Update update) {
        var data = update.getCallbackQuery().getData();
        var userId = update.getCallbackQuery().getFrom().getId();
        var callbackQuery = update.getCallbackQuery();
        var queryId = callbackQuery.getId();
        Long chatId = callbackQuery.getMessage().getChatId();
        Integer messageId = callbackQuery.getMessage().getMessageId();
        if (data != null) {
            handleOldUser(getUser(userId), data);
            dismissMenu(queryId, chatId, messageId);
        }
    }

    private void handleMessage(@NonNull Update update) {
        var message = update.getMessage();
        var user = message.getFrom();
        log.info("{} with id {} wrote {}", user.getFirstName(), user.getId(), message.getText());
        UserDTO userDTO = getUser(user.getId());
        if (userDTO == null) {
            handleNewUser(update);
        } else {
            handleOldUser(userDTO, update);
        }
    }

    private UserDTO getUser(Long userId) {
        try {
            var optional = connectedUsers.stream().filter(userDTO -> userDTO.getId().equals(userId)).findFirst();
            return optional.orElse(null);
        } catch (Exception e) {
            log.error(Strings.ERROR_GETTING_USER, e);
            throw new RuntimeException(e);
        }
    }

    private void handleNewUser(Update update) {
        var message = update.getMessage();
        var user = message.getFrom();
        sendText(user.getId(), Strings.GREETINGS);
        UserDTO newUser = new UserDTO();
        newUser.setId(user.getId());
        connectedUsers.add(newUser);
        handleOldUser(newUser, (String) null);
    }

    private void handleOldUser(UserDTO user, Update update) {
        if (user.getLeftToComplete() == null) {
            handleOldUser(user, update.getMessage().getText());
        } else if (user.getBoaFile() == null) {
            boaFileHandler(user, update);
        } else if (user.getBoletimFile() == null) {
            boletimFileHandler(user, update);
        } else if (user.getContract() == null) {
            contractHandler(user, update.getMessage().getText());
        } else if (user.getContract().getContractFile() == null) {
            contractFileHandler(user, update);
        } else if (user.getContract().getReportFile() == null) {
            reportFileHandler(user, update);
        }
    }

    private void handleOldUser(UserDTO user, String message) {
        if (user == null) {
            log.error(Strings.USER_IS_NULL);
            return;
        }
        if (user.getFullName() == null) {
            fullNameHandler(user, message);
        } else if (user.getDre() == null) {
            dreHandler(user, message);
        } else if (user.getIsRenewal() == null) {
            isRenewalHandler(user, message);
        } else if (user.getRenewalNumber() == null) {
            renewalHandler(user, message);
            craHandler(user, null);
        } else if (user.getRequestNumber() == null) {
            requestHandler(user, message);
            craHandler(user, null);
        } else if (user.getCra() == null) {
            craHandler(user, message);
        } else if (user.getHasCompleted() == null) {
            completedHandler(user, message);
        } else if (user.getLeftToComplete() == null) {
            leftToCompleteHandler(user, message);
        }
    }

    private void userInfoMessage(UserDTO user) {
        sendText(user.getId(), Strings.INFO_HEADER);
        sendText(user.getId(), user.infoConfirmation());
        contractHandler(user, null);
    }

    private void contractHandler(UserDTO user, String message) {
        if (user.getContract().getCompanyName() == null) {
            companyNameHandler(user, message);
        } else if (user.getContract().getCompanyEmail() == null) {
            companyEmailHandler(user, message);
        } else if (user.getContract().getDrawnType() == null) {
            drawnTypeHandler(user, message);
        } else if (user.getContract().getStartDate() == null) {
            startDateHandler(user, message);
        } else if (user.getContract().getEndDate() == null) {
            endDateHandler(user, message);
        }
    }

    private void reportFileHandler(UserDTO user, Update update) {
    }

    private void contractFileHandler(UserDTO user, Update update) {
    }

    private void endDateHandler(UserDTO user, String message) {
    }

    private void startDateHandler(UserDTO user, String message) {
    }

    private void drawnTypeHandler(UserDTO user, String message) {
    }

    private void companyEmailHandler(UserDTO user, String message) {
    }

    private void companyNameHandler(UserDTO user, String message) {
    }

    private void boletimFileHandler(UserDTO user, Update update) {
        if (user.getBoletimFile() == null && update == null) {
            sendText(user.getId(), String.format(Strings.FILE_QUESTION, "Boletim"));
        } else {
            FileDTO dto = handleFile(user, update);
            user.setBoletimFile(dto);
            sendText(user.getId(), Strings.SUCCESS_FILE);
            userInfoMessage(user);
        }
    }

    private void boaFileHandler(UserDTO user, Update update) {
        if (user.getBoaFile() == null && update == null) {
            sendText(user.getId(), String.format(Strings.FILE_QUESTION, "BOA"));
        } else {
            FileDTO dto = handleFile(user, update);
            user.setBoaFile(dto);
            sendText(user.getId(), Strings.SUCCESS_FILE);
            boletimFileHandler(user, null);
        }
    }

    private void leftToCompleteHandler(UserDTO user, String message) {
        if (user.getLeftToComplete() == null && message == null) {
            sendText(user.getId(), Strings.LEFT_TO_COMPLETE_QUESTION);
        } else {
            Integer left = Integer.valueOf(message);
            user.setLeftToComplete(left);
            boaFileHandler(user, null);
        }
    }

    private void completedHandler(UserDTO user, String message) {
        if (user.getHasCompleted() == null && message == null) {
            sendMenu(user.getId(), Strings.COMPLETED_QUESTION, createMenu());
        } else {
            Boolean isCompleted = Boolean.parseBoolean(message);
            user.setHasCompleted(isCompleted);
            if (isCompleted) {
                user.setLeftToComplete(0);
                sendText(user.getId(), String.format(Strings.SELECTED_OPTION, user.getCompleted()));
                boaFileHandler(user, null);
            } else {
                leftToCompleteHandler(user, null);
            }
        }
    }

    private void craHandler(UserDTO user, String message) {
        if (user.getCra() == null && message == null) {
            sendText(user.getId(), Strings.CRA_QUESTION);
        } else {
            if (Validators.isValidCRA(message)) {
                user.setCra(message);
                completedHandler(user, null);
            } else {
                sendText(user.getId(), Strings.INVALID_CRA);
            }
        }
    }

    private void requestHandler(UserDTO user, String message) {
        if (user.getRequestNumber() == null && message == null) {
            sendText(user.getId(), Strings.REQUEST_NUMBER_QUESTION);
        } else {
            user.setRequestNumber(Integer.valueOf(message));
            sendText(user.getId(), String.format(Strings.REQUEST_NUMBER_REPLY, user.getRequestNumber()));
        }
    }

    private void renewalHandler(UserDTO user, String message) {
        if (user.getRenewalNumber() == null && message == null) {
            sendText(user.getId(), Strings.RENEWAL_NUMBER_QUESTION);
        } else {
            user.setRenewalNumber(Integer.valueOf(message));
            sendText(user.getId(), String.format(Strings.RENEWAL_NUMBER_REPLY, user.getRenewalNumber()));
        }
    }

    private void isRenewalHandler(UserDTO user, String message) {
        if (user.getRenewalNumber() == null && message == null) {
            sendMenu(user.getId(), Strings.RENEWAL_QUESTION, createMenu());
        } else {
            Boolean isRenewal = Boolean.parseBoolean(message);
            user.setIsRenewal(isRenewal);
            sendText(user.getId(), String.format(Strings.SELECTED_OPTION, user.getRenewal()));
            if (isRenewal) {
                renewalHandler(user, null);
            } else {
                user.setRenewalNumber(0);
                requestHandler(user, null);
            }
        }
    }

    private void dreHandler(UserDTO user, String message) {
        if (user.getDre() == null && message == null) {
            sendText(user.getId(), String.format(Strings.DRE_INFORM, user.getFirstName()));
        } else {
            if (Validators.isValidDre(message)) {
                user.setDre(message);
                sendText(user.getId(), String.format(Strings.ADDED_DRE, message));
                isRenewalHandler(user, null);
            } else {
                sendText(user.getId(), Strings.INVALID_DRE);
            }
        }
    }

    private void fullNameHandler(UserDTO user, String message) {
        if (user.getFullName() == null && message == null) {
            sendText(user.getId(), Strings.INSERT_FULL_NAME);
        } else {
            if (Validators.isValidName(message)) {
                user.setFullName(message);
                dreHandler(user, null);
            } else {
                sendText(user.getId(), Strings.INVALID_NAME);
            }
        }
    }

    private void sendText(Long who, String what){
        SendMessage sm = SendMessage.builder().chatId(who.toString()).text(what).build();

        try {
            execute(sm);
        } catch (TelegramApiException e) {
            throw new RuntimeException(e);
        }
    }

    private InlineKeyboardMarkup createMenu() {
        var no = InlineKeyboardButton.builder()
                .text(Strings.NO).callbackData(Boolean.FALSE.toString())
                .build();

        var yes = InlineKeyboardButton.builder()
                .text(Strings.YES).callbackData(Boolean.TRUE.toString())
                .build();

        return InlineKeyboardMarkup.builder().keyboardRow(List.of(no, yes)).build();
    }

    public void sendMenu(Long who, String txt, InlineKeyboardMarkup kb){
        SendMessage sm = SendMessage.builder().chatId(who.toString())
                .parseMode("HTML").text(txt)
                .replyMarkup(kb).build();

        try {
            execute(sm);
        } catch (TelegramApiException e) {
            throw new RuntimeException(e);
        }
    }

    private void dismissMenu(String queryId, Long chatId, Integer messageId) {
        AnswerCallbackQuery close = AnswerCallbackQuery.builder()
                .callbackQueryId(queryId).build();
        EditMessageReplyMarkup editMarkup = EditMessageReplyMarkup.builder()
                .chatId(String.valueOf(chatId))
                .messageId(messageId)
                .replyMarkup(null)
                .build();
        try {
            execute(close);
            execute(editMarkup);
        } catch (TelegramApiException e) {
            throw new RuntimeException(e);
        }
    }

    private FileDTO handleFile(UserDTO user, Update update) {
        if (update.hasMessage() && update.getMessage().hasDocument()) {
            var document = update.getMessage().getDocument();
            if (document.getMimeType().equals("application/pdf")) {
                String fileId = document.getFileId();
                String fileName = document.getFileName();
                FileDTO fileDTO = new FileDTO();

                try {
                    var telegramFile = execute(GetFile.builder().fileId(fileId).build());
                    String filePath = telegramFile.getFilePath();
                    File file = downloadFile(filePath);
                    byte[] fileContent = Files.readAllBytes(file.toPath());
                    fileDTO.setFileName(fileName);
                    fileDTO.setFileContent(fileContent);
                    return fileDTO;
                } catch (TelegramApiException | IOException e) {
                    log.error("Error downloading file", e);
                    sendText(user.getId(), Strings.ERROR_WHILE_HANDLING_FILE);
                }
            } else {
                sendText(user.getId(), Strings.INVALID_FILE);
            }
        }
        return null;
    }
}
