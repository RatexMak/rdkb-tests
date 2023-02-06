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
package com.automatics.rdkb.tests.webgui;

import java.awt.image.BufferedImage;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.page.LanSideBasePage;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWebGuiCaptivePortalTest extends AutomaticsTestBase {
	/**
	 *
	 * Test Case : Validate timestamp when SSID and Password changed in captive
	 * portal
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Perform factory reset on a device.</li>
	 * <li>Step 1 : Connect the client to Private Wi-Fi Network and verify
	 * connection status.</li>
	 * <li>Step 2 : Verify LAN GUI admin page is redirected to captive portal page
	 * after factory reset.</li>
	 * <li>Step 3 : Verify log information 'Enter_WiFi_Personalization_captive_mode'
	 * is available in ArmConsolelog.txt.0 log</li>
	 * <li>Step 4 : Verify the private wifi 2.4 and 5 GHz ssid and password can be
	 * configured in captive portal page.</li>
	 * <li>Step 5 : Verify log information 'Exit_WiFi_Personalization_captive_mode'
	 * is available in ArmConsolelog.txt.0 log.</li>
	 * <li>Step 6 : Verify log information 'SSID_name_changed' is available in
	 * WiFilog.txt.0 log</li>
	 * <li>Step 7 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify
	 * connection status.</li>
	 * <li>Step 8 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 9 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 10 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 11 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 12 : Verify disconnecting the client connected with 2.4 GHz SSID in
	 * the setup.</li>
	 * <li>Step 13 : Connect the connected client to 5 GHz Private Wi-Fi Network and
	 * verify connection status.</li>
	 * <li>Step 14 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 15 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 16 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 17 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 18 : Verify disconnecting the client connected with 5 GHz SSID in
	 * the setup</li>
	 * <li>POST-CONDITION 1 : Reactivate the Device.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Govardhan
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-CAPTIVE-PORTAL-5004")
	public void testToVerifyTimestampForCaptivePortal(Dut device) {
		String testCaseId = "TC-RDKB-CAPTIVE-PORTAL-504";
		String errorMessage = null;
		boolean status = false;
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		Dut deviceConnected = null;
		boolean isFactoryResetDone = false;
		boolean isActivationDone = false;
		String searchLogMessage = null;
		String pattenMatcher = null;
		WebDriver driver = null;
		// int preConStepNumber = 1;
		try {
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-CAPTIVE-PORTAL-5004");
			LOGGER.info("TEST DESCRIPTION: Validate timestamp when SSID and Password changed in captive portal.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(" PRE CONDITION 1 : Perform factory reset on a device.");
			LOGGER.info("Step 1 : Connect the client to Private Wi-Fi Network and verify connection status .");
			LOGGER.info("Step 2 : Verify LAN GUI admin page is redirected to captive portal page after factory reset.");
			LOGGER.info(
					"Step 3 : Verify log information 'Enter_WiFi_Personalization_captive_mode' is available in ArmConsolelog.txt.0 log");
			LOGGER.info(
					"Step 4 : Verify the private wifi 2.4 and 5ghz ssid and password can be configured in captive portal page.");
			LOGGER.info(
					"Step 5 : Verify log information 'Exit_WiFi_Personalization_captive_mode' is available in ArmConsolelog.txt.0 log.");
			LOGGER.info("Step 6 : Verify log information 'SSID_name_changed' is available in WiFilog.txt.0 log");
			LOGGER.info("Step 7 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify connection status.");
			LOGGER.info("Step 8 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 9 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 10 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 11 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 12 : Verify disconnecting the client connected with 2.4 GHz SSID in the setup.");
			LOGGER.info(
					"Step 13 : Connect the connected client to 5 GHz Private Wi-Fi Network and verify connection status.");
			LOGGER.info("Step 14 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 15 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 16 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 17 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 18 : Verify disconnecting the client connected with 5 GHz SSID in the setup");
			LOGGER.info(" POST-CONDITION 1 : Reactivate the device.");
			LOGGER.info("***************************************************************************************");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			/**
			 * PRE-CONDITION 1: RETRIEVE PARTNER ID
			 */
			String partnerId = BroadBandPreConditionUtils.executePreConditionToCheckPartnerId(device, tapEnv, 1);
			/**
			 * PRE-CONDITION 2 : PERFORM FACTORY RESET ON THE DEVICE
			 */
			isFactoryResetDone = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv, 2);
			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : CONNECT THE CLIENT TO PRIVATE WI-FI NETWORK AND VERIFY CONNECTION
			 * STATUS
			 * 
			 */
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Connect the client to private wi-fi network and verify connection status ");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : EXECUTE COMMAND : Linux : nmcli dev wifi connect <ssid> password <passwd> WINDOWS : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : Device should be connected with private wi-fi network");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to connect to private wi-fi network";
			try {
				deviceConnected = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (null != deviceConnected);
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 30 SECONDS AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Device has been successfully connected with private wi-fi network");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 2 : VERIFY LAN GUI ADMIN PAGE IS REDIRECTED TO CAPTIVE PORTAL PAGE AFTER
			 * FACTORY RESET.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify lan gui admin page is redirected to captive portal page after factory reset.");
			LOGGER.info("STEP " + stepNumber + " : ACTION : Launch the below url  in browser");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : Capative portal should be launched.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to launch capative portal when accessing lan gui admin page";
			status = LanSideBasePage.isCaptivePortalPageLaunched(tapEnv, device, deviceConnected, partnerId);
			driver = LanSideBasePage.getDriver();
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successful verified captive portal page launched");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * SETP 3 : VERIFY LOG INFORMATION "ENTER_WIFI_PERSONALIZATION_CAPTIVE_MODE" IS
			 * AVAILABLE IN ARMCONSOLELOG.TXT.0
			 */
			stepNumber++;
			boolean isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
			LOGGER.info("IS ATOM SYNC AVAILABLE: " + isAtomSyncAvailable);
			String fileName = isAtomSyncAvailable ? BroadBandTestConstants.RDKLOGS_LOGS_ARM_CONSOLE_0
					: BroadBandTestConstants.RDKLOGS_LOGS_CONSOLE_TXT_0;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.ACTIVATION_ENTER_WIFI_PERSONALIZATION_CAPTIVE, AutomaticsConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateLogMessageInRdkLogs(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, fileName);

			/**
			 * STEP 4 : VERIFY THE PRIVATE WI-FI 2.4 AND 5 GHz SSID AND PASSWORD CAN BE
			 * CONFIGURED IN CAPTIVE PORTAL PAGE.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify the private wifi 2.4 and 5 GHz ssid and password can be configured in captive portal page.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Configure the 2.4 and 5 GHz Private Wi-Fi ssid & Password in the captive portal page");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Private Wi-Fi network should be configured in captive portal.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to configure the 2.4 and 5 GHz Private Wi-Fi ssid & Password in the captive portal page ";
			try {
				status = LanSideBasePage.configureWifiNetworkInCapPortal(tapEnv, device);
			} catch (Exception exception) {
				LOGGER.error(
						"Exception occurred while configuring the private Wi-Fi ssid & Password in the captive portal page."
								+ exception.getMessage());
			} finally {
				saveImageAndCloseDriver(device, testCaseId, stepNum);
				LOGGER.info("Browser closed successfully");
			}
			if (status) {
				isActivationDone = status;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully configured 2.4 and 5 GHz Private Wi-Fi ssid & Password in the captive portal page");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * SETP 5 : VERIFY LOG INFORMATION "EXIT_WIFI_PERSONALIZATION_CAPTIVE_MODE" IS
			 * AVAILABLE IN ARMCONSOLELOG.TXT.0
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.ACTIVATION_EXIT_WIFI_PERSONALIZATION_CAPTIVE, AutomaticsConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateLogMessageInRdkLogs(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, fileName);

			/**
			 * SETP 6 : VERIFY LOG INFORMATION "SSID_NAME_CHANGED" IS AVAILABLE IN
			 * WIFILOG.TXT.0
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.ACTIVATION_SSID_NAME_CHANGE, AutomaticsConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateLogMessageInRdkLogs(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandTestConstants.LOCATION_WIFI_LOG);

			/**
			 * Step 7 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND VERIFY
			 * CONNECTION STATUS AFTER ACTIVATION FROM CAPTIVE PORTAL
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, deviceConnected,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * SETP 8- 11 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, deviceConnected, stepNumber);

			/**
			 * STEP 12 : VERIFY DISCONNECTING THE CLIENT FROM 2.4 GHz PRIVATE WIFI SSID
			 */
			stepNumber = 12;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, deviceConnected,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 13 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS AFTER ACTIVATION FROM CAPTIVE PORTAL
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, deviceConnected,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 14- 17 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, deviceConnected, stepNumber);

			/**
			 * STEP 18 : VERIFY DISCONNECTING THE CLIENT FROM 5 GHz PRIVATE WIFI SSID
			 */
			stepNumber = 18;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, deviceConnected,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING CAPTIVE PORTAL WIFI ACTIVATION LOGS : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			/**
			 * POST-CONDITION 1 : REACTIVATE THE DEVICE
			 */
			if (isFactoryResetDone && !isActivationDone) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				LOGGER.info("POST-CONDITION STEPS");
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
						BroadBandTestConstants.CONSTANT_1);
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
			LOGGER.info("ENDING TEST CASE: TC-RDKB-CAPTIVE-PORTAL-5004");
		}
	}

	/**
	 * Test step method used to validate the log message in rdk log files
	 * 
	 * @param device           instance of{@link Dut}
	 * @param testCaseId       Test case ID
	 * @param stepNumber       Step Number
	 * @param pattenMatcher    Pattern Matcher for Log
	 * @param searchLogMessage Search Message from Log file
	 * @param logFile          Log file to search
	 * @refactor Govardhan
	 */
	public static void executeTestStepToValidateLogMessageInRdkLogs(Dut device, String testCaseId, int stepNumber,
			String pattenMatcher, String searchLogMessage, String logFile) {
		String errorMessage = null;
		boolean status = false;
		String stepNum = "S" + stepNumber;
		String response = null;
		String command = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify log message  '" + searchLogMessage
				+ "' is available in " + logFile);
		LOGGER.info("STEP " + stepNumber + " : ACTION : Grep -i " + searchLogMessage + logFile);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : Expected log message '" + searchLogMessage
				+ "' should be available in " + logFile);
		LOGGER.info("#######################################################################################");
		errorMessage = "Unable to verify log message  '" + searchLogMessage + "' in " + logFile;
		try {
			command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
					BroadBandTestConstants.TEXT_DOUBLE_QUOTE, searchLogMessage,
					BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER, logFile,
					BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.CMD_TAIL_1);
			long startTime = System.currentTimeMillis();
			do {
				response = tapEnv.executeCommandUsingSsh(device, command);
				response = CommonMethods.isNotNull(response)
						&& !response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY) ? response.trim()
								: null;
				status = CommonMethods.isNotNull(response)
						&& CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_FOR_RDKB_LOG_TIMESTAMP)
						&& CommonMethods.patternMatcher(response, pattenMatcher);

			} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

		} catch (Exception e) {
			LOGGER.error(
					"Exception occurred while validating '" + searchLogMessage + "' log message :" + e.getMessage());
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully verified the log message '" + searchLogMessage
					+ "' in " + logFile);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	}

	/**
	 * Method to take screenshot and then close browser
	 * 
	 * @param settop
	 * @param testCaseId
	 * @param stepNum
	 */
	public void saveImageAndCloseDriver(Dut device, String testCaseId, String stepNum) {
		try {
			BufferedImage img = BroadBandWebUiUtils.captureCurrentScreenFromDriver(LanSideBasePage.getDriver());
			if (null != img) {
				String imageName = System.currentTimeMillis() + "_" + "UI_after_" + testCaseId + "_" + stepNum;
				AutomaticsTapApi.saveImages(device, img, imageName);
			}
			LanSideBasePage.closeBrowser();
			LOGGER.info("Browser closed successfully");
		} catch (Exception exception) {
			LOGGER.error("Exception occurred while closing the browser, unable to close browser.");
		}
	}

}
