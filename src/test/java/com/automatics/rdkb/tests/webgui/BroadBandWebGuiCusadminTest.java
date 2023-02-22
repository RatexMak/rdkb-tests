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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWifiBaseTest;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiTestConstant;
import com.automatics.rdkb.webui.page.LanWebGuiLoginPage;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.AutomaticsUtils;
import com.automatics.utils.CommonMethods;

/**
 * Test class for validating the removal of support of CUSADMIN login for
 * Residential devices
 * 
 * @author BALAJI V
 * @refactor Rakesh C N
 */

public class BroadBandWebGuiCusadminTest extends BroadBandWifiBaseTest {

	/**
	 * Verify the password management for WEBUI user for Admin page.
	 * <ol>
	 * <li>Pre-Condition 1: Verify whether WebPA is Up and Running in the
	 * Device.</li>
	 * <li>Pre-Condition 2: Verify the wireless client is connected and having
	 * private 2.4GHz or 5GHz WiFi Capability.</li>
	 * <li>Verify changing password of LAN Admin GUI to Non-Default.</li>
	 * <li>Verify resetting LAN admin GUI password via Webpa.</li>
	 * <li>Verify retrieving the password from Syscfg DB.</li>
	 * <li>Verify retrieving the hashed password from Syscfg DB.</li>
	 * <li>Verify the logging of admin user password reset.</li>
	 * <li>Verify default password change prompt in LAN Admin GUI page.</li>
	 * <li>Verify changing LAN Admin GUI password from WebGui.</li>
	 * <li>Verify retrieving the password from Syscfg DB.</li>
	 * <li>Verify retrieving the hashed password from Syscfg DB.</li>
	 * <li>Verify login to LAN Admin GUI page with changed password.</li>
	 * <li>Verify Factory Resetting the device.</li>
	 * <li>Verify retrieving the password from Syscfg DB.</li>
	 * <li>Verify retrieving the hashed password from Syscfg DB.</li>
	 * <li>Verify connecting client to 2.4GHz or 5GHz WiFi.</li>
	 * <li>Verify default password change prompt in LAN Admin GUI page.</li>
	 * <li>Post-Condition 1: Verify opened browser is closed in connected
	 * client.</li>
	 * <li>Post-Condition 2: Verify reactivating the device.</li>
	 * 
	 * @author BALAJI V
	 * @refactor Rakesh C N
	 * @param device Dut instance
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-PWD-MGMT-1001")
	public void testCusAdminPasswordAfterReset(Dut device) {
		// Variable declaration starts
		String testCaseId = "TC-RDKB-PWD-MGMT-001";
		String stepNum = "S1";
		String errorMessage = "";
		String defaultUserName = null;
		String currentPassword = null;
		boolean status = false;
		boolean isBusinessClass = DeviceModeHandler.isBusinessClassDevice(device);
		boolean isBrowserOpen = false;
		boolean isDeviceReactivationRequire = false;
		Dut clientSettop = null;
		// Variable declaration ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PWD-MGMT-1001");
		LOGGER.info("TEST DESCRIPTION: Verify the password management for WEBUI user for Admin page.");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("Pre-Condition 1: Verify whether WebPA is Up and Running in the Device.");
		LOGGER.info(
				"Pre-Condition 2: Verify the wireless client is connected and having private 2.4GHz or 5GHz WiFi Capability.");
		LOGGER.info("1. Verify changing password of LAN Admin GUI to Non-Default.");
		LOGGER.info("2. Verify resetting LAN admin GUI password via Webpa.");
		LOGGER.info("3. Verify retrieving the password from Syscfg DB.");
		LOGGER.info("4. Verify retrieving the hashed password from Syscfg DB.");
		LOGGER.info("5. Verify the logging of admin user password reset.");
		LOGGER.info("6. Verify default password change prompt in LAN Admin GUI page.");
		LOGGER.info("7. Verify changing LAN Admin GUI password from WebGui.");
		LOGGER.info("8. Verify retrieving the password from Syscfg DB.");
		LOGGER.info("9. Verify retrieving the hashed password from Syscfg DB.");
		LOGGER.info("10. Verify login to LAN Admin GUI page with changed password.");
		LOGGER.info("11. Verfiy Factory Resetting the device.");
		LOGGER.info("12. Verify retrieving the password from Syscfg DB.");
		LOGGER.info("13. Verify retrieving the hashed password from Syscfg DB.");
		LOGGER.info("14. Verify connecting client to 2.4GHz or 5GHz WiFi.");
		LOGGER.info("15. Verify default password change prompt in LAN Admin GUI page.");
		LOGGER.info("Post-Conditon 1: Verify opened browser is closed in connected client.");
		LOGGER.info("Post-Conditon 2: Verify reactivating the device.");

		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			errorMessage = "Webpa is not Up and Running.";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Verify whether WebPA is Up and Running in the Device.");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : Verifying Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED : WebPA should be Up and Running in the Device.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Webpa process is up and running successfully.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}

			errorMessage = "Failed to connect client to WiFi.";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"PRE-CONDITION 2 : DESCRIPTION : Verify the wireless client is connected and having private 2.4GHz or 5GHz WiFi Capability.");
			LOGGER.info("PRE-CONDITION 2 : ACTION : Connect Client to 2.4GHz or 5GHz WiFi.");
			LOGGER.info("PRE-CONDITION 2 : EXPECTED : Client should be connect to any of the WiFi.");
			LOGGER.info("**********************************************************************************");
			clientSettop = BroadBandConnectedClientUtils
					.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			status = null != clientSettop;
			if (status) {
				LOGGER.info("PRE-CONDITION 2 : ACTUAL : Client successfully connected to WiFi.");
			} else {
				LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
			}

			errorMessage = "Unable to set LAN Admin GUI password to Non-Default value.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify changing password of LAN Admin GUI to Non-Default.");
			LOGGER.info("STEP 1: ACTION : Login to LAN Admin page and change the password to Non-Default.");
			LOGGER.info("STEP 1: EXPECTED : LAN Admin GUI password should be set to Non-Default.");
			LOGGER.info("**********************************************************************************");
			isBrowserOpen = status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, clientSettop);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : LAN Admin GUI password successfully set to Non-Default value.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(LanWebGuiLoginPage.getDriver(), tapEnv, device,
					testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S2";
			errorMessage = "Unable to reset LAN admin GUI password via Webpa.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify resetting LAN admin GUI password via Webpa.");
			LOGGER.info("STEP 2: ACTION : Execute the Webpa Set command to reset the LAN admin GUI password.");
			LOGGER.info("STEP 2: EXPECTED : Webpa Set command should return success message.");
			LOGGER.info("**********************************************************************************");
			String webpaParamPassReset = isBusinessClass ? BroadBandWebPaConstants.WEBPA_PARAM_CUSADMIN_PASSWORD_RESET
					: BroadBandWebPaConstants.WEBPA_PARAM_PASSWORD_RESET;
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv, webpaParamPassReset,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : LAN admin GUI password reset was successful via Webpa.");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// ##################################################################################################//
			String defaultPassword = (isBusinessClass ? BroadBandTestConstants.STRING_BUSINESS_CLASS_DEFAULT_ADMIN_PWD
					: BroadBandWebGuiTestConstant.INPUT_TYPE_PASSWORD);
			// Step 3 and 4
			verifyHashAndDefaultPassword(device, testCaseId, BroadBandTestConstants.CONSTANT_3, defaultPassword, false);

			// ##################################################################################################//

			stepNum = "S5";
			errorMessage = "Log message 'Password reset done for cusadmin user' not present in /rdklogs/logs/PAMlog.txt.0 .";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify the logging of admin user password reset.");
			LOGGER.info(
					"STEP 5: ACTION : SSH the device and execute the following command: 'grep -i 'Password reset done for cusadmin user' /rdklogs/logs/PAMlog.txt.0' .");
			LOGGER.info(
					"STEP 5: EXPECTED : Above expected log message should be present in the file /rdklogs/logs/PAMlog.txt.0.");
			LOGGER.info("**********************************************************************************");
			if (isBusinessClass) {
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_CUSADMIN_PWD_RESET,
						BroadBandTestConstants.COMMAND_NTP_LOG_FILE));
				if (status) {
					LOGGER.info(
							"STEP 5: ACTUAL : Log message \"Password reset done for cusadmin user\" is present in /rdklogs/logs/PAMlog.txt.0 .");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			} else {
				LOGGER.info("STEP 5: ACTUAL : Not Applicable for devices other than Business Class devices.");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						" Not Applicable for devices other than Business Class devices", false);
			}

			stepNum = "S6";
			errorMessage = "Failed to prompt message for default password change.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify default password change prompt in LAN Admin GUI page.");
			LOGGER.info("STEP 6: ACTION : Launch LAN Admin GUI page and enter user name and default password.");
			LOGGER.info("STEP 6: EXPECTED : Message for default password change must prompt.");
			LOGGER.info("**********************************************************************************");
			String lanAdminPageurl = AutomaticsTapApi
					.getSTBPropsValue(isBusinessClass ? BroadBandWebGuiTestConstant.ADMIN_PAGE_URL_BUSINESS_CLASS
							: BroadBandWebGuiTestConstant.ADMIN_PAGE_URL);
			status = LanWebGuiLoginPage.validateDefaultPasswordChangePromptInAdminPage(LanWebGuiLoginPage.getDriver(),
					tapEnv, device, clientSettop, lanAdminPageurl);
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Successfully prompt message for default password change.");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(LanWebGuiLoginPage.getDriver(), tapEnv, device,
					testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S7";
			errorMessage = "Unable to change Default password to Non-default.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify changing LAN Admin GUI password from WebGui.");
			LOGGER.info("STEP 7: ACTION : Enter Old and New password and click on change password button.");
			LOGGER.info(
					"STEP 7: EXPECTED : Password must be changed to Non-default and should navigate to Login page.");
			LOGGER.info("**********************************************************************************");
			if (isBusinessClass) {
				defaultUserName = tapEnv.executeCommandUsingSsh(device,
						BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_USERNAME_BUSINESS_CLASS);
				currentPassword = tapEnv.executeCommandUsingSsh(device,
						BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_PASSWORD_BUSINESS_CLASS);
			} else {
				defaultUserName = tapEnv.executeCommandUsingSsh(device,
						BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_USERNAME);
				currentPassword = tapEnv.executeCommandUsingSsh(device,
						BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_PASSWORD);
				if (CommonMethods.isNull(currentPassword)) {
					currentPassword = BroadBandTestConstants.STRING_PASSWORD;
				}
			}
			String newPassword = AutomaticsTapApi.getSTBPropsValue(BroadBandWebGuiTestConstant.ADMIN_PAGE_PASSWORD);
			status = LanWebGuiLoginPage.changePasswordFromPasswordChangePrompt(device, tapEnv, currentPassword,
					newPassword);
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : Default value of LAN Admin Gui password changed to Non-default successfully.");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(LanWebGuiLoginPage.getDriver(), tapEnv, device,
					testCaseId, stepNum, status, errorMessage, true);

			// ##################################################################################################//
			// Step 8 and 9
			verifyHashAndDefaultPassword(device, testCaseId, BroadBandTestConstants.CONSTANT_8, newPassword, false);
			LanWebGuiLoginPage.closeBrowser();
			// ##################################################################################################//

			stepNum = "S10";
			errorMessage = "Unable to login LAN Admin GUI page with changed password.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify login to LAN Admin GUI page with changed password.");
			LOGGER.info("STEP 10: ACTION : Launch LAN Admin WebGUI URL and login with changed password.");
			LOGGER.info("STEP 10: EXPECTED : LAN Admin GUI login page must be lauched and logged in successfully.");
			LOGGER.info("**********************************************************************************");
			status = LanWebGuiLoginPage.logintoLanPageinConnectedClient(tapEnv, device, clientSettop, lanAdminPageurl,
					defaultUserName, newPassword);
			if (status) {
				LOGGER.info("STEP 10: ACTUAL : LAN Admin GUI page is launched and logged in successfully.");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(LanWebGuiLoginPage.getDriver(), tapEnv, device,
					testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S11";
			errorMessage = "Failed to Factory Resetting the device.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify Factory Resetting the device.");
			LOGGER.info(
					"STEP 11: ACTION : Execute the Webpa Set command for following param: Device.X_CISCO_COM_DeviceControl.FactoryReset and set value as 'Router,Wifi,VoIP,Dect,MoCA' .");
			LOGGER.info(
					"STEP 11: EXPECTED : Webpa Set command should return success message and device should go for Factory Resetting.");
			LOGGER.info("**********************************************************************************");
			isDeviceReactivationRequire = BroadBandFactoryResetUtils.methodToPerformFactoryResetObjectAndDeviceToComeUp(
					tapEnv, device, BroadBandTestConstants.STRING_FOR_FACTORY_RESET_OF_THE_DEVICE);
			if (isDeviceReactivationRequire) {
				status = BroadBandCommonUtils.verifyFactoryResetReasonFromBootTimeLog(device, tapEnv);
				AutomaticsUtils.sleep(AutomaticsConstants.FIVE_MINUTES);
				LOGGER.info("BEGIN BROAD BAND DEVICE REACTIVATION.");
				BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
			}
			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Factory Resetting device was successful and device came up.");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// ##################################################################################################//
			// Step 12 and 13
			verifyHashAndDefaultPassword(device, testCaseId, BroadBandTestConstants.CONSTANT_12, defaultPassword, true);
			// ##################################################################################################//

			stepNum = "S14";
			errorMessage = "Failed to connect client to WiFi.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Verify connecting client to 2.4GHz or 5GHz WiFi.");
			LOGGER.info("STEP 14: ACTION : Connect Client to 2.4GHz or 5GHz WiFi.");
			LOGGER.info("STEP 14: EXPECTED : Client should be connect to any of the WiFi.");
			LOGGER.info("**********************************************************************************");
			clientSettop = BroadBandConnectedClientUtils
					.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			status = null != clientSettop;
			if (status) {
				LOGGER.info("STEP 14: ACTUAL : Client successfully connected to WiFi.");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S15";
			errorMessage = "Failed to disable Captive portal after factory reset.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 15: DESCRIPTION : Verify default password change prompt in LAN Admin GUI page.");
			LOGGER.info("STEP 15: ACTION : Launch LAN Admin GUI page and enter user name and default password.");
			LOGGER.info("STEP 15: EXPECTED : Message for default password change must prompt.");
			LOGGER.info("**********************************************************************************");
			// Disabling captive portal
			LOGGER.info("Going to disable captive portal");
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			LOGGER.info("Is captive portal disabled successfully - " + status);
			if (status) {
				errorMessage = "Failed to prompt message for default password change.";
				status = LanWebGuiLoginPage.validateDefaultPasswordChangePromptInAdminPage(
						LanWebGuiLoginPage.getDriver(), tapEnv, device, clientSettop, lanAdminPageurl);
			}
			if (status) {
				LOGGER.info("STEP 15: ACTUAL : Successfully prompt message for default password change.");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(LanWebGuiLoginPage.getDriver(), tapEnv, device,
					testCaseId, stepNum, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			int postConditionStepNum = 0;
			if (isBrowserOpen) {
				postConditionStepNum++;
				status = false;
				errorMessage = "Failed to close opened browser in connected client.";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ ": DESCRIPTION : Verify opened browser is closed in connected client.");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ ": ACTION : Close opened browser in connected client.");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ ": EXPECTED : Opened browser should be closed in connected client.");
				LOGGER.info("**********************************************************************************");
				try {
					LanWebGuiLoginPage.closeBrowser();
					status = true;
				} catch (Exception e) {
					errorMessage = errorMessage + e.getMessage();
				}
				if (status) {
					LOGGER.info("POST-CONDITION " + postConditionStepNum
							+ ": ACTUAL : Opened browser is closed in connected client.");
				} else {
					LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + errorMessage);
				}
			}

			if (isDeviceReactivationRequire) {
				postConditionStepNum++;
				status = false;
				errorMessage = "Failed to reactivate the device.";
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"POST-CONDITION " + postConditionStepNum + ": DESCRIPTION : Verify reactivating the device.");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ ": ACTION : Reactivate the device using SNMP or Webpa.");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ ": EXPECTED : Device reactivation should be successful.");
				LOGGER.info("**********************************************************************************");
				try {
					status = BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
				} catch (Exception e) {
					errorMessage = errorMessage + e.getMessage();
				}
				if (status) {
					LOGGER.info(
							"POST-CONDITION " + postConditionStepNum + ": ACTUAL : Device reactivated successfully.");
				} else {
					LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + errorMessage);
				}
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PWD-MGMT-1001");
	}

	/**
	 * Method to verify hash and default password for the test case
	 * TC-RDKB-PWD-MGMT-1001
	 * 
	 * @param device              Dut instance
	 * @param testCaseId          TestCaseId
	 * @param stepNumber          step number of the test case id
	 * @param passwordToVerify    password to be verified.
	 * @param isAfterFactoryReset true, to verify after factory reset
	 * @author Praveenkumar Paneerselvam
	 * @refactor Rakesh C N
	 */
	private void verifyHashAndDefaultPassword(Dut device, String testCaseId, int stepNumber, String passwordToVerify,
			boolean isAfterFactoryReset) {

		LOGGER.info("STEP " + (stepNumber) + ": DESCRIPTION : Verify retrieving the password from Syscfg DB");
		LOGGER.info("STEP " + (stepNumber) + ": ACTION : Execute command: syscfg get user_password_2");
		LOGGER.info("STEP " + (stepNumber) + ": EXPECTED : Default password value \"highspeed\" should be return");

		String stepNum = "s" + stepNumber;

		String errorMessage = "Failed to get default password using the command syscfg get user_password_2";
		boolean executionStatus = false;
		executionStatus = BroadBandCommonUtils.verifyAdminPagePasswordFromSyscfgCommand(tapEnv, device,
				passwordToVerify);
		LOGGER.info("STEP " + (stepNumber) + ": ACTUAL :  "
				+ (executionStatus ? "Successfully verified default password of the Admin page" : errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, executionStatus, errorMessage, false);

		// ##################################################################################################//

		LOGGER.info("STEP " + (++stepNumber) + ": DESCRIPTION : Verify retrieving the hashed password from Syscfg DB");
		LOGGER.info("STEP " + (stepNumber) + ": ACTION : Execute command: syscfg get hash_password_2");
		if (isAfterFactoryReset) {
			LOGGER.info("STEP " + (stepNumber) + ": EXPECTED : Hashed value of the " + passwordToVerify
					+ " should be empty or null.");
		} else {
			LOGGER.info("STEP " + (stepNumber) + ": EXPECTED : Hashed value of the " + passwordToVerify
					+ " should be returned.");
		}

		stepNum = "s" + stepNumber;
		executionStatus = false;
		if (isAfterFactoryReset) {
			errorMessage = "Hash password is not empty or null from the command syscfg get hash_password_2";

			if (!DeviceModeHandler.isRPIDevice(device)) {
				String command = DeviceModeHandler.isBusinessClassDevice(device)
						? BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_HASH_PASSWORD_BUSINESS_CLASS
						: BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_HASH_PASSWORD;
				executionStatus = CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device, command));
			} else {
				String command = BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_HASH_PASSWORD;
				executionStatus = CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device, command));
			}
		} else {
			errorMessage = "Failed to get hash password using the command syscfg get hash_password_2";
			executionStatus = BroadBandCommonUtils.verifyAdminPageHashPasswordFromSyscfgCommand(tapEnv, device,
					passwordToVerify);
		}
		LOGGER.info("STEP " + (stepNumber) + ": ACTUAL :  "
				+ (executionStatus ? "Successfully verified default password of the Admin page" : errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, executionStatus, errorMessage, false);

	}
}