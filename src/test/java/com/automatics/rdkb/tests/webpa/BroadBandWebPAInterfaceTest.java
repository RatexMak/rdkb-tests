/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.webpa;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.enums.ReportingServiceParameterEnum;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.LoggerUtils;
import com.automatics.rdkb.utils.ParodusUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWebPAInterfaceTest extends AutomaticsTestBase {
    /**
     *
     * Test Case # 1: Verify there are no reconnection of iot & config clients after successful connection.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) verify SESHAT_URL from device.properties.</li>
     * <li>S2) verify PARODUS_URL from device.properties.</li>
     * <li>S3) Verify that seshat library is initializing with updated SESHAT_URL.</li>
     * <li>S4) Verify that seshat discovered url is updated PARODUS_URL in PARODUS log..</li>
     * <li>S5) Verify iptable INPUT rule to accept <4090 iptable interface> interface connections".</li>
     * <li>S6) Verify iptable FORWARD rule to accept <4090 iptable interface> interface connections.</li>
     * <li>S7) Verify communication between parodus and webpa is through 4090 interfaces.</li>
     * <li>S8) Enable IOT service using WebPA command and verify that Webpa are using <4090 interface> interface ip to
     * register with parodus.</li>
     * <li>S9) Verify whether Webpa is able to communicate with parodus using set request..</li>
     * <li>S10) Verify whether Webpa is able to communicate with parodus using get request..</li>
     * <li>S11) Verify whether xsmart is able to communicate with parodus.</li>
     * <li>S12) Enable lmlite report.</li>
     * <li>S13) Verify lmlite reports delivery to parodus.</li>
     * <li>S14) Enable harvester report</li>
     * <li>S15) Verify harvester reports delivery to parodus.</li>
     * </ol>
     * 
     * @author Praveen Kumar P
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-INTERFACE-1101")
    public void testWebpaInterface(Dut device) {

	String testCaseId = "TC-RDKB-INTERFACE-101";
	String stepNumber = "s1";
	boolean status = false;
	String errorMessage = null;
	Dut clientConnected = null;

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    // Precondition - Reboot Device
	    LOGGER.info(" Precondition - Device Rebooting ");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (!status) {
		errorMessage = " Unable to reboot the device.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    LOGGER.info(" Waiting for LMlite log to be initialized after reboot. Maximum wait time 10 min");
	    long pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
	    long startTime = System.currentTimeMillis();
	    StringBuffer sbCommand = new StringBuffer();
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadBandTraceConstants.LOG_MESSAGE_LMLITE_INITIALIZATION);
	    sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    sbCommand.append(BroadBandCommandConstants.LOG_FILE_LMLITE);
	    do {
		LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		status = CommonUtils.searchLogFiles(tapEnv, device, sbCommand.toString());
	    } while ((System.currentTimeMillis() - startTime) < pollDuration && !status);

	    // In order to check the InterfaceDevicesWifi report, there should a client client to device .
	    clientConnected = BroadBandConnectedClientUtils
		    .get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    if (null != clientConnected) {
		LOGGER.info("Successfully connected a client to 2.4/5ghz SSID");
	    } else {
		LOGGER.info("Failed to connect a client to 2.4/5ghz SSID");
	    }
	    String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    /**
	     * Step 1 : verify SESHAT_URL from device.properties.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Step 1 : Description : verify SESHAT_URL from device.properties");
	    LOGGER.info("Step 1 : Action : verify SESHAT_URL from device.properties");
	    LOGGER.info("Step 1: Expected Result - SESHAT_URL should use updated URL <updated SESHAT_URL>");
	    LOGGER.info("**********************************************************************************");
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    String parodusIP = AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.PARODUS_IP_PROPERTY_FILE_KEY);
	    String seshatIP = AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.SESHAT_IP_PROPERTY_FILE_KEY);
	    String parodusPort = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PARODUS_PORT_PROPERTY_FILE_KEY);
	    String seshatPort = AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.SESHAT_PORT_PROPERTY_FILE_KEY);
	    StringBuffer url = new StringBuffer();
	    url.append(BroadBandTestConstants.TCP_PROTOCOL);
	    url.append(BroadBandTestConstants.DELIMITER_COLON);
	    url.append(BroadBandTestConstants.FORWARD_SLASH_CHARACTER);
	    url.append(BroadBandTestConstants.FORWARD_SLASH_CHARACTER);
	    url.append(seshatIP);
	    url.append(BroadBandTestConstants.DELIMITER_COLON);
	    url.append(seshatPort);
	    String seshatInterfaceUrl = url.toString();

	    url = new StringBuffer();
	    url.append(BroadBandTestConstants.TCP_PROTOCOL);
	    url.append(BroadBandTestConstants.DELIMITER_COLON);
	    url.append(BroadBandTestConstants.FORWARD_SLASH_CHARACTER);
	    url.append(BroadBandTestConstants.FORWARD_SLASH_CHARACTER);
	    url.append(parodusIP);
	    url.append(BroadBandTestConstants.DELIMITER_COLON);
	    url.append(parodusPort);
	    String parodusInterfaceUrl = url.toString();

	    // Get SESHAT_URL from /etc/device.properties
	    LOGGER.info("seshatInterfaceUrl is : " + seshatInterfaceUrl);
	    LOGGER.info("parodusInterfaceUrl is : " + parodusInterfaceUrl);
	    String seshatUrl = BroadBandCommonUtils.getPropertyFromDeviceProperties(device, tapEnv,
		    BroadBandTestConstants.STRING_SESHAT_URL, BroadBandTestConstants.REGEX_SESHAT_URL);
	    LOGGER.info("seshatUrl is : " + seshatUrl);
	    status = CommonMethods.isNotNull(seshatUrl) && seshatUrl.contains(seshatInterfaceUrl);
	    errorMessage = "SESHAT_URL is not updated with interface 4090.";
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: Seshat Url from device.properties " + seshatUrl);
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    /**
	     * Step 2 : verify PARODUS_URL from device.properties.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Step 2 :Description: verify PARODUS_URL from device.properties");
	    LOGGER.info("Step 2 :Action: verify PARODUS_URL from device.properties");
	    LOGGER.info("Step 2: Expected Result - PARODUS_URL should use updated URL <updated PARODUS_URL>.");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s2";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    // Get PARODUS_URL from /etc/device.properties
	    String parodusUrl = ParodusUtils.getParodusUrl(tapEnv, device);
	    status = CommonMethods.isNotNull(parodusUrl) && parodusUrl.contains(parodusInterfaceUrl);
	    errorMessage = "PARODUS_URL is not updated with interface 4090.";
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL: PARODUS_URL is updated with " + parodusUrl);
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 3 : Verify iptable INPUT rule to accept <iptable 4090 interface> interface connections
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Step 3 :Description: Verify iptable INPUT rule to accept "
		    + BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables() + " interface connections.");
	    LOGGER.info("Step 3 :Action: Verify iptable INPUT rule to accept "
		    + BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables() + " connections.");
	    LOGGER.info("Step 3: Expected Result - The result should not be null or empty.");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s3";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    // Creating command to be executed using String buffer
	    sbCommand = new StringBuffer();
	    sbCommand.append(BroadBandCommandConstants.CMD_IPTABLES_S);
	    sbCommand.append(BroadBandTestConstants.SYMBOL_PIPE);
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables());
	    sbCommand.append(BroadBandTestConstants.SYMBOL_PIPE);
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadBandTestConstants.STRING_INPUT);
	    sbCommand.append(BroadBandTestConstants.SYMBOL_PIPE);
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadBandTestConstants.STRING_ACCEPT);
	    // Command to validate iptables INPUT rule to check 4090 interface connnections. /usr/sbin/iptables -S|grep
	    // -i <iptable 4090 interface> |grep -i INPUT |grep -i ACCEPT
	    LOGGER.info("Command for validation - " + sbCommand.toString());
	    String response = tapEnv.executeCommandUsingSsh(device, sbCommand.toString());
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTER_MATCHER_IPTABLE_INPUT_RULE);
	    errorMessage = "iptable INPUT rule for "
		    + BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables()
		    + " interface is not in Accept state. ";
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: iptable INPUT rule for "
			+ BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables()
			+ " interface is in Accept state.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    /**
	     * Step 4 : Verify iptable FORWARD rule to accept <iptable 4090 interface> interface connections.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Step 4 : Description : Verify iptable FORWARD rule to accept "
		    + BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables() + " interface connections.");
	    LOGGER.info("Step 4: Expected Result - The result should not be null or empty.");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s4";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    // Creating command to be executed using String buffer
	    sbCommand = new StringBuffer();
	    sbCommand.append(BroadBandCommandConstants.CMD_IPTABLES_S);
	    sbCommand.append(BroadBandTestConstants.SYMBOL_PIPE);
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables());
	    sbCommand.append(BroadBandTestConstants.SYMBOL_PIPE);
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadBandTestConstants.STRING_FORWARD);
	    sbCommand.append(BroadBandTestConstants.SYMBOL_PIPE);
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadBandTestConstants.STRING_ACCEPT);
	    // Command to validate iptables FORWARD rule to check 4090 interface connnections. /usr/sbin/iptables
	    // -S|grep -i <iptable 4090 interface> |grep -i FORWARD |grep -i ACCEPT
	    LOGGER.info("Command for validation - " + sbCommand.toString());
	    response = tapEnv.executeCommandUsingSsh(device, sbCommand.toString());
	    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
		    BroadBandTestConstants.PATTERN_MATCHER_IPTABLE_FORWARD_RULE);
	    errorMessage = "iptable FORWARD rule for "
		    + BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables()
		    + " interface is not in Accept state.";
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL: iptable FORWARD rule for "
			+ BroadbandPropertyFileHandler.get4090InterfaceToValidateInIptables()
			+ " interface is  in Accept state.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 5 : Verify communication between parodus and webpa is through 4090 interfaces.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "Step 5 : Description :Verify communication between parodus and webpa is through 4090 interfaces.");
	    LOGGER.info("Step 5: Expected Result - Verify the entries are in ESTABLISHED state.");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s5";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    sbCommand = new StringBuffer();
	    // Creating command to be executed using String buffer
	    sbCommand.append(BroadBandCommandConstants.CMD_NETSTAT_AN);
	    sbCommand.append(BroadBandTestConstants.SYMBOL_PIPE);
	    sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadBandTestConstants.DOUBLE_QUOTE);
	    sbCommand.append(parodusIP);
	    sbCommand.append(BroadBandTestConstants.DELIMITER_COLON);
	    sbCommand.append(BroadBandTestConstants.STRING_6666);
	    sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    sbCommand.append(BroadBandTestConstants.ASTERISK);
	    sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    sbCommand.append(seshatIP);
	    sbCommand.append(BroadBandTestConstants.DOUBLE_QUOTE);
	    // Command to validate interface 4090 established between parodus and webpa
	    response = tapEnv.executeCommandUsingSsh(device, sbCommand.toString());
	    LOGGER.info("SB Command : " + sbCommand.toString());
	    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
		    BroadBandTestConstants.PATTERN_MATCHER_CONNECTION_ESTABLISHED);
	    errorMessage = "Communication between parodus and webpa is not established through 4090 interface.";
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL: entries are in ESTABLISHED state.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 6 : Enable IOT service using WebPA command and Verify that Webpa is using <4090 interface> interface
	     * ip to register with parodus
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "Step 6 : Description :Enable IOT service using WebPA command .Verify that Webpa is using <4090 interface> interface ip to register with parodus.");
	    LOGGER.info(
		    "Step 6: Expected Result - IOT service should be enabled and the Log message should have updated SESHAT_URL in the format - client service iot is added to list with url: tcp://<<Seshat_IP>>:<port>.");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s6";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    // Enabling IOT sevice using WebPA command and validating success response.
	    status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_ENABLE_IOT,
		    BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("Enabled IOT service using webPA -" + status);
	    errorMessage = "Failed to enable IOT service";
	    if (status) {
		sbCommand = new StringBuffer();
		sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
		sbCommand.append(BroadBandTraceConstants.LOG_MESSAGE_CLIENT_SERVICE_IOT);
		sbCommand.append(seshatIP);
		sbCommand.append(BroadBandTestConstants.DELIMITER_COLON);
		sbCommand.append(BroadBandTestConstants.IOT_CLIENT_PORT_6668);
		sbCommand.append(BroadBandTestConstants.DOUBLE_QUOTE);
		sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
		sbCommand.append(BroadBandCommandConstants.LOG_FILE_PARODUS);
		// Command to validate expected log message in /rdklogs/logs/PARODUSlog.txt.0
		LOGGER.info("Command for validation - " + sbCommand.toString());
		status = CommonUtils.searchLogFiles(tapEnv, device, sbCommand.toString());
		errorMessage = "Communication between Webpa and clients is not using 4090. interface";
	    }
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL: IOT service should be enabled and the Log message should have updated SESHAT_URL in the format - client service iot is added to list with url: tcp://<<Seshat_IP>>:<port>.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    /**
	     * Step 7 : Verify whether Webpa is able to communicate with parodus using set request.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "Step 7 : Description : Verify whether Webpa is able to communicate with parodus using set request.");
	    LOGGER.info("Step 7: Expected Result - WEBPA requests should return success response.");
	    LOGGER.info("WebPA set request for " + ReportingServiceParameterEnum.NETWORK_DEVICE_STATUS
		    + " is validated in automation. Other WEBPA (POST/GET/SET) parameters are to be covered by CATR request for regression");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s7";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    String webPASetValue = BroadBandTestConstants.FALSE;
	    // Disable LMlite NetworkDeviceStatus report using WebPA set request.
	    String webPASetRequest = ReportingServiceParameterEnum.NETWORK_DEVICE_STATUS.getWebpaEnable();
	    status = BroadBandWiFiUtils.setWebPaParams(device, webPASetRequest, webPASetValue,
		    BroadBandTestConstants.CONSTANT_3);
	    errorMessage = " Error in setting value through webPa. WebPa property name -" + webPASetRequest;
	    // If WebPA request fails, remaining steps are not executed as other steps are dependent on WebPA request .
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL: Disable report using WEBPA command is " + status
			+ ". WebPA parameter used is " + webPASetRequest);
	    } else {
		LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    /**
	     * Step 8 : Verify whether Webpa is able to communicate with parodus using get request.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "Step 8 : Description : Verify whether Webpa is able to communicate with parodus using get request.");
	    LOGGER.info("Step 8: Expected Result - WEBPA requests should return polling value set in previous step.");
	    LOGGER.info("WebPA get request for " + ReportingServiceParameterEnum.NETWORK_DEVICE_STATUS
		    + " is validated in automation. Other WEBPA (POST/GET/SET) parameters are to be covered by CATR request for regression");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s8";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    response = tapEnv.executeWebPaCommand(device, webPASetRequest);
	    status = CommonMethods.isNotNull(response) && response.equals(webPASetValue);
	    errorMessage = "Failed to get response through WebPA get for parameter " + webPASetRequest
		    + ". WebPA response -" + response;
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL: WebPA get request response for parameter " + webPASetRequest + " is "
			+ response);
	    } else {
		LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    /**
	     * Step 9 : Enable lmlite report.
	     */
	    stepNumber = "s9";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "Step 9 : Description :Enable lmlite report .Set polling and reporting period of the lmlite parameter to 30.");
	    LOGGER.info(
		    "Step 9 : Expected Result - All three parameters should be set and WebPA request should respond with success message and log message\"LMLite: Network Status Report STARTED\" must be present in LM.txt.0 log file.");
	    LOGGER.info("**********************************************************************************");
	    // Network device parameter enables LMlite report logging.Enabling network device parameter service with
	    // polling and reporting period values as 30.
	    status = BroadBandCommonUtils.enableDisableReportingService(device,
		    ReportingServiceParameterEnum.NETWORK_DEVICE_STATUS, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.STRING_30, BroadBandTestConstants.STRING_30);
	    errorMessage = "Failed to enable LMlite report ";
	    if (status) {
		LOGGER.info(" Waiting for 20 sec to get required log in LM.txt.0");
		tapEnv.waitTill(BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		sbCommand = new StringBuffer();
		sbCommand.append(BroadBandTestConstants.GREP_COMMAND);
		sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
		sbCommand.append(BroadBandTraceConstants.LOG_MESSAGE_LMLITE_NETWORK_STATUS_REPORT_STARTED);
		sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
		sbCommand.append(BroadBandCommandConstants.LOG_FILE_LM);
		LOGGER.info("Command for validation - " + sbCommand.toString());
		status = CommonUtils.searchLogFiles(tapEnv, device, sbCommand.toString());
		errorMessage = "Expected log message \"LMLite: Network Status Report STARTED\" not found in LM.txt.0 log file.";
	    }
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL:Expected log message \\\"LMLite: Network Status Report STARTED\\\"  found in LM.txt.0 log file.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    /**
	     * Step 10 : Verify lmlite reports delivery to parodus.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Step 10 : Description:  Verify lmlite reports delivery to parodus.");
	    LOGGER.info("Step 10 :Expected Result - Log message should be present in Parodus log.");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s10";
	    errorMessage = "Log message not should be present in Parodus log.";

	    startTime = System.currentTimeMillis();
	    do {
		response = LoggerUtils.getLatestLogMessageBasedOnTimeStamp(tapEnv, device,
			BroadBandTestConstants.LOG_MESSAGE_RECEIVED_UPSTREAM_EVENT_NETWORK_STATUS_REPORT,
			BroadBandCommandConstants.LOG_FILE_PARODUS, timeStamp, false);
		status = CommonMethods.isNotNull(response);
	    } while (((System.currentTimeMillis() - startTime) < Integer.valueOf(BroadBandTestConstants.STRING_30)
		    * 1000) && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL:Log message should be present in Parodus log.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 11 : Enable harvester report. Set polling and reporting period of the harvester parameter to 30.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "Step 11 :Description:  Enable harvester report. Set polling and reporting period of the harvester parameter to 30.");
	    LOGGER.info(
		    "Step 11 : Expected Result - All three parameters should be set and WebPA request should respond with success message.");
	    stepNumber = "s11";
	    LOGGER.info("**********************************************************************************");
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    // Interface device wifi parameter enables harvester report logging.Enabling Interface Device wifi service
	    // with polling and reporting period values as 30.
	    status = BroadBandCommonUtils.enableDisableReportingService(device,
		    ReportingServiceParameterEnum.INTERFACE_DEVICE_WIFI, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.STRING_30, BroadBandTestConstants.STRING_30);
	    LOGGER.info("Enabled harvester report throught webPA command - " + status);
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL:Enabled harvester report throught webPA command - " + status);
	    } else {
		LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    /**
	     * Step 12 : Verify harvester reports delivery to parodus.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Step 12: Description: Verify harvester reports delivery to parodus.");
	    LOGGER.info("Step 12: Expected Result - Log message should be present in Parodus log.");
	    LOGGER.info("**********************************************************************************");
	    stepNumber = "s12";
	    status = false;
	    errorMessage = BroadBandTestConstants.EMPTY_STRING;
	    sbCommand = new StringBuffer();
	    errorMessage = "ecm mac not found.";

	    startTime = System.currentTimeMillis();
	    do {
		response = LoggerUtils.getLatestLogMessageBasedOnTimeStamp(tapEnv, device,
			BroadBandTestConstants.LOG_MESSAGE_RECEIVED_UPSTREAM_EVENT_INERFACE_DEVICES_WIFI,
			BroadBandCommandConstants.LOG_FILE_PARODUS, timeStamp, false);
		status = CommonMethods.isNotNull(response);
	    } while (((System.currentTimeMillis() - startTime) < Integer.valueOf(BroadBandTestConstants.STRING_30)
		    * 1000) && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL:Log message should be present in Parodus log.");
	    } else {
		LOGGER.error("STEP 12: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VERIFYING 4090 INTERFACE USAGE IN WEBPA COMMUNICATION : " + errorMessage);
	    LOGGER.info("**********************************************************************************");
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);

	} finally {

	    if (null != clientConnected) {
		// disconnect the client from Gateway Device
		BroadBandResultObject result = BroadBandConnectedClientUtils.disconnectCnnClientFromSsid(tapEnv, device,
			clientConnected);
		if (result.isStatus()) {
		    LOGGER.info("Disconnected the client from Gateway Device SSID");
		} else {
		    LOGGER.error("Failed to disconnect the client from Gateway Device SSID");
		}
	    }
	    // Setting default value for both harvester and lmlite reporting
	    LOGGER.info("POST-CONDITION : Disbling LMlite and Harvester report");
	    try {
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_WIFI_REPORT, BroadBandTestConstants.FALSE,
			BroadBandTestConstants.CONSTANT_3)
			&& BroadBandWiFiUtils.setWebPaParams(device,
				BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT,
				BroadBandTestConstants.FALSE, BroadBandTestConstants.CONSTANT_3);
		if (status) {
		    LOGGER.info("Disabled Network status and Interface devices report");
		} else {
		    LOGGER.error("Failed to Disable Network status and Interface devices report");
		}
	    } catch (Exception e) {
		LOGGER.error("Exception while disabling the reports  in post condition ");
	    }

	}
	LOGGER.info("################### ENDING TEST CASE - TC-RDKB-INTERFACE-1101 ###################");
    }
}
