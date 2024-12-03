package com.eucappelli;

import com.eucappelli.enums.ContractField;
import com.eucappelli.enums.DrawnType;
import com.eucappelli.enums.RequestStatus;
import com.eucappelli.enums.UserField;
import com.eucappelli.models.ContractDTO;
import com.eucappelli.models.FileDTO;
import com.eucappelli.models.RequestDTO;
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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class Bot extends TelegramLongPollingBot {
    private final List<UserDTO> connectedUsers = new ArrayList<>();
    private final List<RequestDTO> requests = new ArrayList<>();
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
            log.info("Requests: {}", requests);
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
            UserDTO user = getUser(userId);
            if (user.getBoletimFile() != null && user.getContract() == null) {
                userInfoMessage(user, data);
            } else {
                handleOldUser(user, update);
            }
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
            if (update.getMessage().isCommand()) {
                handleCommands(userDTO, update);
            } else {
                handleOldUser(userDTO, update);
            }
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

    private List<RequestDTO> getRequests(Long userId) {
        return requests.stream().filter(requestDTO -> requestDTO.getUserId().equals(userId)).toList();
    }

    private RequestDTO getRequest(Long userId) {
        try {
            var optional = requests.stream().filter(requestDTO -> requestDTO.getUserId().equals(userId)).findFirst();
            return optional.orElse(null);
        } catch (Exception e) {
            log.error(Strings.ERROR_GETTING_REQUEST, e);
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
        String message = update == null ? null :
                update.getMessage() != null ? update.getMessage().getText() : update.getCallbackQuery().getData();
        if (!user.allInfoDone()) {
            handleOldUser(user, message);
        } else if (user.getBoaFile() == null) {
            boaFileHandler(user, update);
        } else if (user.getBoletimFile() == null) {
            boletimFileHandler(user, update);
        } else if (!user.allContractDone()) {
            contractHandler(user, message);
        } else if (user.getContract().getContractFile() == null) {
            contractFileHandler(user, update);
        } else if (getRequests(user.getId()).isEmpty()) {
            contractInfoMessage(user, message);
        } else {
            handleRequests(user);
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
        } else if (user.getCra() == null) {
            craHandler(user, message);
        } else if (user.getLeftToComplete() == null) {
            leftToCompleteHandler(user, message);
        }
    }

    private void handleRequests(UserDTO user) {
        sendText(user.getId(), Strings.STATUS_TEXT);
        sendText(user.getId(), Strings.INFO_TEXT);
        sendText(user.getId(), Strings.EDIT_INFO_TEXT);
        sendText(user.getId(), Strings.EDIT_CONTRACT_TEXT);
        sendText(user.getId(), Strings.REJECTED_WARNING_TEXT);
        sendText(user.getId(), Strings.ACCEPTED_WARNING_TEXT);
    }

    private void handleCommands(UserDTO user, Update update) {
        String message = update.getMessage().getText();
        switch (message) {
            case "/status" -> {
                RequestDTO requestDTO = getRequest(user.getId());
                sendText(user.getId(), requestDTO.requestInfo());
            }
            case "/allinfo" -> {
                RequestDTO requestDTO = getRequest(user.getId());
                sendText(user.getId(), requestDTO.getUser().infoConfirmation());
                sendText(user.getId(), requestDTO.getUser().getContract().contractConfirmation());
            }
            case "/editinfo" -> userInfoMessage(user, null);
            case "/editcontract" -> contractInfoMessage(user, null);
            case null, default -> handleOldUser(user, (Update) null);
        }
    }

    private void userInfoMessage(UserDTO user, String message) {
        if (message == null) {
            Map<String, String> map = getInfoMap();
            sendText(user.getId(), Strings.INFO_HEADER);
            sendText(user.getId(), user.infoConfirmation());
            sendMenu(user.getId(), Strings.CONFIRMATION_QUESTION, createMultipleMenu(map));
        } else {
            UserField field = UserField.valueOf(message);
            if (field != UserField.NONE) {
                switch (field) {
                    case BOA -> user.setBoaFile(null);
                    case BOLETIM -> user.setBoletimFile(null);
                    case CRA -> user.setCra(null);
                    case DRE -> user.setDre(null);
                    case FULLNAME -> user.setFullName(null);
                    case RENEWAL -> user.setIsRenewal(null);
                    case LEFT_TO_COMPLETE -> user.setLeftToComplete(null);
                }
            }
            handleOldUser(user, (Update) null);
        }
    }

    private static Map<String, String> getInfoMap() {
        Map<String, String> map = new HashMap<>();
        map.put(Strings.NONE, UserField.NONE.name());
        map.put(Strings.FULL_NAME, UserField.FULLNAME.name());
        map.put(Strings.DRE, UserField.DRE.name());
        map.put(Strings.RENEWAL, UserField.RENEWAL.name());
        map.put(Strings.CRA, UserField.CRA.name());
        map.put(Strings.LEFT_TO_COMPLETE, UserField.CRA.name());
        map.put(Strings.BOA, UserField.BOA.name());
        map.put(Strings.BOLETIM, UserField.BOLETIM.name());
        return map;
    }

    private void contractInfoMessage(UserDTO user, String message) {
        if (message == null) {
            Map<String, String> map = getContractMap();
            sendText(user.getId(), Strings.INFO_HEADER);
            sendText(user.getId(), user.getContract().contractConfirmation());
            sendMenu(user.getId(), Strings.SEND_QUESTION, createMultipleMenu(map));
        } else {
            ContractField field = ContractField.valueOf(message);
            if (field != ContractField.SEND) {
                switch (field) {
                    case COMPANY_NAME -> user.getContract().setCompanyName(null);
                    case COMPANY_EMAIL -> user.getContract().setCompanyEmail(null);
                    case DRAWN_BY -> user.getContract().setDrawnType(null);
                    case START_DATE -> user.getContract().setStartDate(null);
                    case END_DATE -> user.getContract().setEndDate(null);
                    case CONTRACT_FILE -> user.getContract().setContractFile(null);
                }
                handleOldUser(user, (Update) null);
            } else {
                Date date = new Date();
                RequestDTO requestDTO = new RequestDTO();
                requestDTO.setId(date.getTime());
                requestDTO.setDate(date);
                requestDTO.setStatus(RequestStatus.SENT);
                requestDTO.setUserId(user.getId());
                requestDTO.setUser(user);
                sendText(user.getId(), Strings.REQUEST_SENT);
                requests.add(requestDTO);
                handleOldUser(user, (Update) null);
            }
        }
    }

    private static Map<String, String> getContractMap() {
        Map<String, String> map = new HashMap<>();
        map.put(Strings.SEND, ContractField.SEND.name());
        map.put(Strings.COMPANY_NAME, ContractField.COMPANY_NAME.name());
        map.put(Strings.EMAIL, ContractField.COMPANY_EMAIL.name());
        map.put(Strings.DRAWN_BY, ContractField.DRAWN_BY.name());
        map.put(Strings.START_DATE, ContractField.START_DATE.name());
        map.put(Strings.END_DATE, ContractField.END_DATE.name());
        map.put(Strings.CONTRACT, ContractField.CONTRACT_FILE.name());
        return map;
    }

    private void contractHandler(UserDTO user, String message) {
        if (user.getContract() == null) {
            user.setContract(new ContractDTO());
        }
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

    private void contractFileHandler(UserDTO user, Update update) {
        if (user.getContract().getContractFile() == null && update == null) {
            sendText(user.getId(), Strings.CONTRACT_FILE_QUESTION);
        } else {
            FileDTO dto = handleFile(user, update);
            if (dto != null) {
                user.getContract().setContractFile(dto);
                sendText(user.getId(), Strings.SUCCESS_FILE);
                handleOldUser(user, (Update) null);
            }
        }
    }

    private void endDateHandler(UserDTO user, String message) {
        if (user.getContract().getEndDate() == null && message == null) {
            sendText(user.getId(), Strings.END_DATE);
        } else {
            if (Validators.isValidDate(message)) {
                user.getContract().setEndDate(message);
                handleOldUser(user, (Update) null);
            } else {
                sendText(user.getId(), Strings.INVALID_DATE);
            }
        }
    }

    private void startDateHandler(UserDTO user, String message) {
        if (user.getContract().getStartDate() == null && message == null) {
            sendText(user.getId(), Strings.START_DATE);
        } else {
            if (Validators.isValidDate(message)) {
                user.getContract().setStartDate(message);
                handleOldUser(user, (Update) null);
            } else {
                sendText(user.getId(), Strings.INVALID_DATE);
            }
        }
    }

    private void drawnTypeHandler(UserDTO user, String message) {
        if (user.getContract().getDrawnType() == null && message == null) {
            Map<String, String> drawnTypes = new HashMap<>();
            drawnTypes.put(Strings.COMPANY, DrawnType.COMPANY.name());
            drawnTypes.put(Strings.AGENCY, DrawnType.AGENCY.name());
            drawnTypes.put(Strings.UNKNOWN, DrawnType.UNKNOWN.name());
            sendMenu(user.getId(), Strings.DRAWN_BY_QUESTION, createMultipleMenu(drawnTypes));
        } else {
            DrawnType drawnType = DrawnType.valueOf(message);
            user.getContract().setDrawnType(drawnType);
            handleOldUser(user, (Update) null);
        }
    }

    private void companyEmailHandler(UserDTO user, String message) {
        if (user.getContract().getCompanyEmail() == null && message == null) {
            sendText(user.getId(), Strings.COMPANY_EMAIL_QUESTION);
        } else {
            if (Validators.isValidEmail(message)) {
                user.getContract().setCompanyEmail(message);
                handleOldUser(user, (Update) null);
            } else {
                sendText(user.getId(), Strings.INVALID_EMAIL);
            }
        }
    }

    private void companyNameHandler(UserDTO user, String message) {
        if (user.getContract().getCompanyName() == null && message == null) {
            sendText(user.getId(), Strings.COMPANY_NAME_QUESTION);
        } else {
            user.getContract().setCompanyName(message);
            handleOldUser(user, (Update) null);
        }
    }

    private void boletimFileHandler(UserDTO user, Update update) {
        if (user.getBoletimFile() == null && update == null) {
            sendText(user.getId(), String.format(Strings.FILE_QUESTION, "Boletim"));
        } else {
            FileDTO dto = handleFile(user, update);
            if (dto != null) {
                user.setBoletimFile(dto);
                sendText(user.getId(), Strings.SUCCESS_FILE);
                userInfoMessage(user, null);
            }
        }
    }

    private void boaFileHandler(UserDTO user, Update update) {
        if (user.getBoaFile() == null && update == null) {
            sendText(user.getId(), String.format(Strings.FILE_QUESTION, "BOA"));
        } else {
            FileDTO dto = handleFile(user, update);
            if (dto != null) {
                user.setBoaFile(dto);
                sendText(user.getId(), Strings.SUCCESS_FILE);
                handleOldUser(user, (Update) null);
            }
        }
    }

    private void leftToCompleteHandler(UserDTO user, String message) {
        if (user.getLeftToComplete() == null && message == null) {
            sendText(user.getId(), Strings.LEFT_TO_COMPLETE_QUESTION);
        } else {
            Integer left = Integer.valueOf(message);
            user.setLeftToComplete(left);
            handleOldUser(user, (Update) null);
        }
    }

    private void craHandler(UserDTO user, String message) {
        if (user.getCra() == null && message == null) {
            sendText(user.getId(), Strings.CRA_QUESTION);
        } else {
            if (Validators.isValidCRA(message)) {
                user.setCra(message);
                handleOldUser(user, (Update) null);
            } else {
                sendText(user.getId(), Strings.INVALID_CRA);
            }
        }
    }

    private void isRenewalHandler(UserDTO user, String message) {
        if (user.getIsRenewal() == null && message == null) {
            sendMenu(user.getId(), Strings.RENEWAL_QUESTION, createConfirmationMenu());
        } else {
            Boolean isRenewal = Boolean.parseBoolean(message);
            user.setIsRenewal(isRenewal);
            sendText(user.getId(), String.format(Strings.SELECTED_OPTION, user.getRenewal()));
            handleOldUser(user, (Update) null);
        }
    }

    private void dreHandler(UserDTO user, String message) {
        if (user.getDre() == null && message == null) {
            sendText(user.getId(), String.format(Strings.DRE_INFORM, user.getFirstName()));
        } else {
            if (Validators.isValidDre(message)) {
                user.setDre(message);
                sendText(user.getId(), String.format(Strings.ADDED_DRE, message));
                handleOldUser(user, (Update) null);
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
                handleOldUser(user, (Update) null);
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

    private List<InlineKeyboardButton> createRow(String text, String callbackData) {
        List<InlineKeyboardButton> row = new ArrayList<>();
        row.add(InlineKeyboardButton.builder()
                .text(text)
                .callbackData(callbackData)
                .build());
        return row;
    }

    private InlineKeyboardMarkup createMultipleMenu(Map<String, String> options) {
        List<List<InlineKeyboardButton>> rows = new ArrayList<>();
        
        for (Map.Entry<String, String> entry : options.entrySet()) {
            List<InlineKeyboardButton> row = createRow(entry.getKey(), entry.getValue());
            rows.add(row);
        }

        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    private InlineKeyboardMarkup createConfirmationMenu() {
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
        sendText(user.getId(), Strings.PDF_WARNING);
        return null;
    }
}
