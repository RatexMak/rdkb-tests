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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.Browser;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.enums.BroadBandWhixEnumConstants.WEBPA_AP_INDEXES;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWifiBaseTest;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.BroadBandWebUiBaseTest;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiElements;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiTestConstant;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiTestConstant.RestoreGateWay;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.webui.page.LanWebGuiLoginPage;
import com.automatics.rdkb.webui.page.BroadBandCommonPage;
import com.automatics.rdkb.webui.page.BroadBandResetRestoreGatewayPage;
import com.automatics.rdkb.webui.page.BroadbandLocalIpConfigurationPage;
import com.automatics.rdkb.webui.page.LanSideBasePage;
import com.automatics.rdkb.webui.page.LanSidePageNavigation;
import com.automatics.rdkb.webui.page.LanSideWiFiPage;
import com.automatics.rdkb.webui.page.LanSideWizardPage;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.webui.page.BroadBandAtGlancePage;
import com.automatics.rdkb.webui.page.BroadBandBasePage;
import com.automatics.rdkb.webui.page.BroadBandResetRestoreGatewayPage;

public class BroadBandWebGuiAdminPageTest extends BroadBandWifiBaseTest {

	/** Constant holds the browser status flag **/
	private static boolean isBrowserOpen = false;

	/** Constant holds the test step number **/
	private static String stepNum = "";

	/** Constant holds the error message **/
	private static String errorMessage = "";

	/** Constant holds the status **/
	private static boolean status = false;

	/**
	 *
	 * Test Case : Verify the password validations when client connected to 2.4GHz
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 2.4 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 2.4 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>Step 1 : Launch Broad band User Admin page login page and verify login
	 * status</li>
	 * <li>Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and
	 * verify navigation status</li>
	 * <li>Step 3 : Verify the home network password cannot be set as blank</li>
	 * <li>Step 4 : Verify the home network password cannot be set with space in
	 * password</li>
	 * <li>Step 5 : Verify the home network password cannot be set with special
	 * character in password</li>
	 * <li>Step 6 : Verify the home network password cannot be set with password
	 * mismatch</li>
	 * <li>POST-CONDITION 1 : Verify disconnecting the 2.4 GHz private wifi
	 * SSID</li>
	 * <li>POST-CONDITION 2 : Close the Browser LAN Client</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Preetha
	 * @refactor Athira
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-2GHZ-HOME-NTW-PSWD-1001")
	public void testToVerifyPasswordValidations2GHz(Dut device) {
		Dut deviceConnectedWith2Ghz = null;
		// Variable Declaration begins
		String stepNum = "S1";
		String errorMessage = "";
		boolean status = false;
		isBrowserOpen = false;
		String testCaseId = "TC-RDKB-2GHZ-HOME-NTW-PSWD-101";
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-2GHZ-HOME-NTW-PSWD-1001");
		LOGGER.info("TEST DESCRIPTION: Verify the password validations when client connected to 2.4GHz");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify IPv4 is assigned on the Ethnernet client");
		LOGGER.info("PRE-CONDITION 3 : Verify IPv6 is assigned on the Ethnernet client");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
		LOGGER.info("Step 1 : Verify login into the LAN GUI Adimn page");
		LOGGER.info(
				"Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and verify navigation status");
		LOGGER.info("Step 3 : Verify the home network password cannot be set as blank");
		LOGGER.info("Step 4 : Verify the home network password cannot be set with space in password");
		LOGGER.info("Step 5 : Verify the home network password cannot be set with special character  in password");
		LOGGER.info("Step 6 : Verify the home network password cannot be set with password mismatch ");
		LOGGER.info("POST-CONDITION 1 : Verify disconnecting the 2.4 GHz private wifi SSID");
		LOGGER.info("POST-CONDITION 2 : Close the Browser LAN Client");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnectedWith2Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
					tapEnv, BroadBandTestConstants.BAND_2_4GHZ);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			executeTestStepToValidatePasswordFields(device, tapEnv, deviceConnectedWith2Ghz, testCaseId);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-condition 1 : DISCONNECT THE 2.4 GHZ PRIVATE WIFI SSID
			 */
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith2Ghz, BroadBandTestConstants.CONSTANT_1);
			/**
			 * Post-condition 2 : CLOSE THE LAN SIDE BROWSER
			 */
			LOGGER.info("####################################################################################");
			LOGGER.info("POST-CONDITION 2 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 2 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 2 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("###################################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 2 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 2 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 2 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-2GHZ-HOME-NTW-PSWD-1001");
	}

	/**
	 *
	 * Test Case : Verify the password validations when client connected to 5GHz
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 5 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 5 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 5 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>Step 1 : Launch Broad band User Admin page login page and verify login
	 * status</li>
	 * <li>Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and
	 * verify navigation status</li>
	 * <li>Step 3 : Verify the home network password cannot be set as blank</li>
	 * <li>Step 4 : Verify the home network password cannot be set with space in
	 * password</li>
	 * <li>Step 5 : Verify the home network password cannot be set with special
	 * character in password</li>
	 * <li>Step 6 : Verify the home network password cannot be set with password
	 * mismatch</li>
	 * <li>POST-CONDITION 1 : Verify disconnecting the 5 GHz private wifi SSID</li>
	 * <li>POST-CONDITION 2 : Close the Browser LAN Client</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Preetha
	 * @refactor Govardhan
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-5GHZ-HOME-NTW-PSWD-1002")
	public void testToVerifyPasswordValidations5GHz(Dut device) {
		Dut deviceConnectedWith5Ghz = null;
		// Variable Declaration begins
		String stepNum = "S1";
		String errorMessage = "";
		boolean status = false;
		isBrowserOpen = false;
		String testCaseId = "TC-RDKB-5GHZ-HOME-NTW-PSWD-102";
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-5GHZ-HOME-NTW-PSWD-1002");
		LOGGER.info("TEST DESCRIPTION: Verify the password validations when client connected to 5 GHz");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 5 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
		LOGGER.info("PRE-CONDITION 3 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
		LOGGER.info("Step 1 : Verify login into the LAN GUI Adimn page");
		LOGGER.info(
				"Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and verify navigation status");
		LOGGER.info("Step 3 : Verify the home network password cannot be set as blank");
		LOGGER.info("Step 4 : Verify the home network password cannot be set with space in password");
		LOGGER.info("Step 5 : Verify the home network password cannot be set with special character  in password");
		LOGGER.info("Step 6 : Verify the home network password cannot be set with password mismatch ");
		LOGGER.info("POST-CONDITION 1 : Verify disconnecting the 5 GHz private wifi SSID");
		LOGGER.info("POST-CONDITION 2 : Close the Browser LAN Client");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnectedWith5Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
					tapEnv, BroadBandTestConstants.BAND_5GHZ);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			executeTestStepToValidatePasswordFields(device, tapEnv, deviceConnectedWith5Ghz, testCaseId);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-condition 1 : DISCONNECT THE 5 GHZ PRIVATE WIFI SSID
			 */
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith5Ghz, BroadBandTestConstants.CONSTANT_1);
			/**
			 * Post-condition 2 : CLOSE THE LAN SIDE BROWSER
			 */
			LOGGER.info("####################################################################################");
			LOGGER.info("POST-CONDITION 2 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 2 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 2 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("###################################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 2 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 2 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 2 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-5GHZ-HOME-NTW-PSWD-1002");
	}

	/**
	 *
	 * Test Case : Verify the password validations when client connected to LAN
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to Ethernet and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with Ethernet</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with Ethernet</li>
	 * <li>PRE-CONDITION 4 : Verify internet is accessible by using Interface IPv4
	 * on the ethernet client</li>
	 * <li>PRE-CONDITION 5 : Verify internet is accessible by using Interface IPv6
	 * on the ethernet client</li>
	 * <li>Step 1 : Launch Broad band User Admin page login page and verify login
	 * status</li>
	 * <li>Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and
	 * verify navigation status</li>
	 * <li>Step 3 : Verify the home network password cannot be set as blank</li>
	 * <li>Step 4 : Verify the home network password cannot be set with space in
	 * password</li>
	 * <li>Step 5 : Verify the home network password cannot be set with special
	 * character in password</li>
	 * <li>Step 6 : Verify the home network password cannot be set with password
	 * mismatch</li>
	 * <li>POST-CONDITION 1 : Close the Browser LAN Client</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Preetha
	 * @refactor Govardhan
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-LAN-HOME-NTW-PSWD-1003")
	public void testToVerifyPasswordValidationsUsingLAN(Dut device) {
		Dut deviceConnectedWithLAN = null;
		// Variable Declaration begins
		String stepNum = "S1";
		String errorMessage = "";
		boolean status = false;
		isBrowserOpen = false;
		String testCaseId = "TC-RDKB-LAN-HOME-NTW-PSWD-103";
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-LAN-HOME-NTW-PSWD-1003");
		LOGGER.info("TEST DESCRIPTION: Verify the password validations when client connected to LAN");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to Ethernet and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with Ethernet");
		LOGGER.info("PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with Ethernet");
		LOGGER.info("PRE-CONDITION 4 : Verify internet is accessible by using Interface IPv4  on the ethernet client");
		LOGGER.info("PRE-CONDITION 5 : Verify internet is accessible by using Interface IPv6  on the ethernet client");
		LOGGER.info("Step 1 : Verify login into the LAN GUI Adimn page");
		LOGGER.info(
				"Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and verify navigation status");
		LOGGER.info("Step 3 : Verify the home network password cannot be set as blank");
		LOGGER.info("Step 4 : Verify the home network password cannot be set with space in password");
		LOGGER.info("Step 5 : Verify the home network password cannot be set with special character  in password");
		LOGGER.info("Step 6 : Verify the home network password cannot be set with password mismatch ");
		LOGGER.info("POST-CONDITION 1 : Close the Browser LAN Client");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnectedWithLAN = BroadBandPreConditionUtils.executePreConditionToVerifyLanClientStatus(device,
					tapEnv, BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			executeTestStepToValidatePasswordFields(device, tapEnv, deviceConnectedWithLAN, testCaseId);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-condition 1 : CLOSE THE LAN SIDE BROWSER
			 */
			LOGGER.info("####################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("###################################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-LAN-HOME-NTW-PSWD-1003");
	}

	/**
	 * Test method to validate the password validations in Connected client
	 * 
	 * @param device          {@link Dut}
	 * @param tapEnv          instance of {@link AutomaticsTapApi}
	 * @param deviceConnected instance of connected device
	 * @param testCaseId      Test case ID
	 * @refactor Govardhan
	 */
	private static void executeTestStepToValidatePasswordFields(Dut device, AutomaticsTapApi tapEnv,
			Dut deviceConnected, String testCaseId) {
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		WebDriver webDriver = null;
		isBrowserOpen = false;
		String currentPassword = "";
		LanSideWizardPage lanSideWizardPage = null;
		LanSidePageNavigation lanSidePageNavigation = null;
		int stepNumber = 1;
		stepNum = "S" + stepNumber;

		/**
		 * STEP 1 : LAUNCH ADMIN LOGIN PAGE AND VERIFY LOGIN STATUS
		 */
		errorMessage = "Unable to Login to LanGUI page using Admin credential";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Launch Broad band User Admin page login page and verify login status");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Naviagte to Admin login page using admin/password credentials");
		LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : partner page should be launched and login should be successful using admin credentials");
		LOGGER.info("**********************************************************************************");
		try {
			status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
			isBrowserOpen = status;
			webDriver = LanWebGuiLoginPage.getDriver();
			lanSidePageNavigation = new LanSidePageNavigation(webDriver);
			lanSideWizardPage = new LanSideWizardPage();
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + ": ACTUAL : User admin page has been launched and logged in successfully");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		/**
		 * Step 2 : NAVIGATE TO THE GATEWAY > HOME NETWORK WIZARD - STEP 1 PAGE AND
		 * VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > HOME NETWORK WIZARD - STEP 1 PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > HOME NETWORK WIZARD - STEP 1");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > HOME NETWORK WIZARD - STEP 1 PAGE");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > HOME NETWORK WIZARD - STEP 1 PAGE";
		try {
			status = lanSidePageNavigation.navigateToWizardStep1Page(device, tapEnv, webDriver);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP" + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > HOME NETWORK WIZARD - STEP 1 PAGE");
		} else {
			LOGGER.error("STEP" + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		/**
		 * Step 3 : VERIFY THE HOME NETWORK PASSWORD CANNOT BE SET AS BLANK
		 */

		stepNumber++;
		stepNum = "S" + stepNumber;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " :  DESCRIPTION : Verify the home network password cannot be set as blank");
		LOGGER.info("STEP " + stepNumber + " :  ACTION : Leave the New Password as blank and click on Save button");
		LOGGER.info(
				"STEP " + stepNumber + " :  EXPECTED : Error message should be displayed as This is a required field.");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to verify the home network password cannot be set as blank";
		try {
			status = lanSideWizardPage.validatePasswordFieldsWithBlank();
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully verified, home network password cannot be set as blank");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 4 : VERIFY THE HOME NETWORK PASSWORD CANNOT BE SET WITH SPACE IN
		 * PASSWORD
		 */

		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "Unable to verify the home network password cannot be set with space in password";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : Verify the home network password cannot be set with space in password");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : Enter the New Password  with space in password and click on Save button");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : Error message should be displayed as Only letters and numbers are valid. No spaces or special characters.");
		LOGGER.info("**********************************************************************************");
		try {
			currentPassword = DeviceModeHandler.isBusinessClassDevice(device)
					? tapEnv.executeCommandUsingSsh(device,
							BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_PASSWORD_BUSINESS_CLASS)
					: tapEnv.executeCommandUsingSsh(device, BroadBandWebGuiTestConstant.SSH_GET_DEFAULT_PASSWORD);
			status = lanSideWizardPage.validateNewPasswordFieldWithSpaceOrSpecialChar(tapEnv, currentPassword,
					BroadbandPropertyFileHandler.getTheHomeNetworkPasswordWithSpace());
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully verified, home network password cannot be set with space in password");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 5 : VERIFY THE HOME NETWORK PASSWORD CANNOT BE SET WITH WITH SPECIAL
		 * CHARACTER IN PASSWORD
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "Unable to verify the home network password cannot be set with special character in password";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : Verify the home network password cannot be set with special character in password");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : Enter the New Password  with special character in password and click on Save button");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : Error message should be displayed as Only letters and numbers are valid. No spaces or special characters.");
		LOGGER.info("**********************************************************************************");
		try {
			status = lanSideWizardPage.validateNewPasswordFieldWithSpaceOrSpecialChar(tapEnv, currentPassword,
					BroadbandPropertyFileHandler.getTheHomeNetworkPasswordWithSpecialCharacter());
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully verified, home network password cannot be set with special character in password");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 6 : VERIFY THE HOME NETWORK PASSWORD CANNOT BE SET WITH PASSWORD
		 * MISMATCH
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "Unable to verify the home network password cannot be set with password mismatch";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : Verify the home network password cannot be set with password mismatch");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : Enter the Re-Enter New Password  with differnet value and click on Save button");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : Error message should be displayed as Please enter the same value again.");
		LOGGER.info("**********************************************************************************");
		try {
			status = lanSideWizardPage.validateHomeNetworkPasswordMismatch(tapEnv);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully verified, home network password cannot be set with password mismatch");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
	}

	/**
	 *
	 * Test Case : Verify Login into admin page using default password and Verify
	 * Default values in Firewall IPV4, IPV6 and Managed Devices Pages.
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Perform factory reset on the device</li>
	 * <li>PRE-CONDITION 2 : Reactivate the gateway device</li>
	 * <li>PRE-CONDITION 3 : Disable MESH, AKER AND CLOUD UI on gateway</li>
	 * <li>Step 1 : Connect the client setup to 2.4/5 GHZ SSID and verify connection
	 * status</li>
	 * <li>Step 2 : Verify the correct IPv4 address for client connected with 2.4/5
	 * GHz SSID</li>
	 * <li>Step 3 : Verify the correct IPv6 address for client connected with 2.4/5
	 * GHz SSID</li>
	 * <li>Step 4 : Verify the internet connectivity in the connected wifi client
	 * using ipv4 interface</li>
	 * <li>Step 5 : Verify the internet connectivity in the connected wifi client
	 * using ipv6 interface.</li>
	 * <li>Step 6 : Verify login into the lan gui adimn page by using valid userid
	 * and default password</li>
	 * <li>Step 7 : Verify login into the lan gui adimn page by using valid userid
	 * and valid password</li>
	 * <li>Step 8 : Launch the IPv4 Firewall page from partner page</li>
	 * <li>Step 9 : Verify the Default IPv4 firewall mode is set to Minimum Security
	 * (Low)</li>
	 * <li>Step 10 : Launch the IPv6 FireWall page from partner page</li>
	 * <li>Step 11 : Verify the Default IPv6 firewall mode is set to Typical
	 * Security (default)</li>
	 * <li>Step 12 : Launch the Managed Devices under Parental Control page from
	 * partner page</li>
	 * <li>Step 13 : Verify the default configurations in Managed devices page</li>
	 * <li>POST-CONDITION 1 : Verify lan side browser closed</li>
	 * <li>POST-CONDITION 2 : Reactivate the gateway device</li>
	 * <li>POST-CONDITION 3 : Enable CloudUI Enable and Mesh mode via XPC API</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Said Hisham
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-WIFI-DFLT-ADM-LOGIN-5001")
	public void testToVerifyAdminPageAccessUsingDefaultPassword(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-DFLT-ADM-LOGIN-501";
		int stepNumber = BroadBandTestConstants.CONSTANT_1;
		String stepNum = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnected = null;
		WebDriver driver = null;
		BroadBandResultObject broadBandResultObject = null;
		boolean isBusinessClassDevice = DeviceModeHandler.isBusinessClassDevice(device);
		boolean isFactoryReset = false;
		boolean isReactivated = false;
		int preConStepNumber = BroadBandTestConstants.CONSTANT_1;
		int postConStepNumber = BroadBandTestConstants.CONSTANT_1;
		String meshInitialStatus = null;
		// Variable Declaration Ends
		try {
			LOGGER.info("#####################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-DFLT-ADM-LOGIN-5001");
			LOGGER.info("TEST DESCRIPTION: 15247");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION 1 : Perform factory reset on the device");
			LOGGER.info("PRE-CONDITION 2 : Reactivate the gateway device");
			LOGGER.info("PRE-CONDITION 3 : Disable MESH, AKER AND CLOUD UI on gateway");
			LOGGER.info("Step 1 : Connect the client setup to 2.4/5 GHZ SSID and verify connection status");
			LOGGER.info("Step 2 : Verify the correct IPv4 address for client connected with 2.4/5 GHz SSID");
			LOGGER.info("Step 3 : Verify the correct IPv6 address for client connected with 2.4/5 GHz SSID");
			LOGGER.info("Step 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
			LOGGER.info("Step 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface.");
			LOGGER.info("Step 6 : Verify login into the lan gui adimn page by using valid userid and default password");
			LOGGER.info("Step 7 : Verify login into the lan gui adimn page by using valid userid and valid password");
			LOGGER.info("Step 8. Launch the IPv4 Firewall page from partner page");
			LOGGER.info("Step 9. Verify the Default IPv4 firewall mode is set to Minimum Security (Low)");
			LOGGER.info("Step 10. Launch the IPv6 FireWall page from partner page");
			LOGGER.info("Step 11. Verify the Default IPv6 firewall mode is set to Typical Security (default)");
			LOGGER.info("Step 12. Launch the Managed Devices under Parental Control page from partner page");
			LOGGER.info("Step 13. Verify the default configurations in Managed devices page ");
			LOGGER.info("POST-CONDITION 1 : Verify lan side browser closed");
			LOGGER.info("POST-CONDITION 2 : Reactivate the gateway device");
			LOGGER.info("#####################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("############################# STARTING PRE-CONFIGURATIONS #############################");
			/**
			 * PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE
			 */
			isFactoryReset = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
					preConStepNumber);
			/**
			 * PRE-CONDITION 2 : REACTIVATE THE ROUTER DEVICE
			 */
			preConStepNumber++;
			isReactivated = BroadBandPreConditionUtils.executePreConditionToReacitivateDevice(device, tapEnv,
					preConStepNumber);

			LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

			/**
			 * SETP 1 : Connect the client setup to 2.4/5 GHZ SSID
			 */
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Connect the client to 2.4/5 GHZ private WI-FI network and verify connection status ");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Connect to 2.4/5 GHZ Wifi using below commands LINUX : nmcli dev wifi connect <ssid> password <passwd> WINDOWS : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Device should be connected with 2.4/5 GHZ private wi-fi network");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to connect to 2.4/5 GHZ private WI-FI network";
			try {
				deviceConnected = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (null != deviceConnected);
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Device has been connected with 2.4/5 GHZ private WI-FI network");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 2 : VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO GET THE CORRECT IPV4 ADDRESS FROM CLIENT";
			String osType = ((Device) deviceConnected).getOsType();
			status = BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					deviceConnected, tapEnv);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV4 ADDRESS FROM CLIENT");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 3 : VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP " + stepNumber + ": DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv6 Address' or LINUX : ifconfig | grep 'inet6 ' ON THE CONNECTED CLIENT");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS");
				LOGGER.info("**********************************************************************************");
				errorMessage = "UNABLE TO GET THE CORRECT IPV6 ADDRESS FROM CLIENT";
				status = BroadBandConnectedClientUtils
						.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType, deviceConnected, tapEnv);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV6 ADDRESS FROM CLIENT");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step...");
				tapEnv.updateExecutionForAllStatus(device, testId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 4 : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID
			 * INTERFACE USING IPV4 .
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED USING IPV4 ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4 ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT WITH USING IPV4 ";
			broadBandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
					tapEnv, deviceConnected,
					BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
					BroadBandTestConstants.IP_VERSION4);
			status = broadBandResultObject.isStatus();
			errorMessage = broadBandResultObject.getErrorMessage();
			if (!status) {
				errorMessage = "PIGN OPERATION FAILED TO ACCESS THE SITE 'www.google.com' USING IPV4 ";
				status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(deviceConnected, tapEnv,
						BroadBandTestConstants.PING_TO_GOOGLE, BroadBandTestConstants.IP_VERSION4);
			}
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY USING IPV4 ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 5 : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID
			 * INTERFACE USING IPV6 .
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED USING IPV6");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v 'www.google.com' | grep '200 OK' OR ping -6 -n 5 google.com , LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -6 -n 5 google.com ON THE CONNECTED CLIENT");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4 ");
				LOGGER.info("**********************************************************************************");
				errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT WITH USING IPV6";
				broadBandResultObject = BroadBandConnectedClientUtils
						.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv, deviceConnected,
								BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
								BroadBandTestConstants.IP_VERSION6);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
				if (!status) {
					errorMessage = "PIGN OPERATION FAILED TO ACCESS THE SITE 'www.google.com' USING IPV6 ";
					status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(deviceConnected, tapEnv,
							BroadBandTestConstants.PING_TO_GOOGLE, BroadBandTestConstants.IP_VERSION6);
				}
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + " : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY USING IPV6");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step...");
				tapEnv.updateExecutionForAllStatus(device, testId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * Step 6 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING VALID USERID AND
			 * DEFAULT PASSWORD
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			driver = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify the gateway admin page is accessible in connected client and can be logged in using valid userid and default password");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Launch the gateway admin gui in browser , once the page is loaded ,use valid userid and default password to login");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Gateway admin page should be accessible from client and can be able to login using valid userid and default password");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to login admin page in connected client using valid userid and default password";
			if (isBusinessClassDevice) {
				try {
					status = LanWebGuiLoginPage.logintoLanPageUsingDefaultPassWord(tapEnv, device, deviceConnected);
					driver = LanWebGuiLoginPage.getDriver();
					LOGGER.info("webDriver " + driver);
				} catch (Exception e) {
					errorMessage += e.getMessage();
					LOGGER.error("Exception occurred during Gateway Admin Page login : " + errorMessage);
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Lan gui admin login successful using valid userid and default password");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum,
						status, errorMessage, false);
			} else {
				LOGGER.info("STEP :" + stepNumber
						+ " : ACTUAL :Verify login into the lan gui adimn page by using valid userid and default password is not applicable for residential devices");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.NA_MSG_FOR_RESIDENTIAL_CLASS_DEVICES, false);
			}
			/**
			 * Step 7 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING VALID USERID AND
			 * VALID PASSWORD
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Unable to login admin page in connected client";
			driver = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify the gateway admin page is accessible in connected client and can be logged in using cusadmin/****** credential for commercial devices");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Launch the gateway admin gui in browser , once the page is loaded ,use cusadmin/****** for commercial devices to login");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Gateway admin page should be accessible from client and can be able to login using cusadmin/****** credential for commercial devices");
			LOGGER.info("**********************************************************************************");
			try {
				status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
				isBrowserOpen = status;
				driver = LanWebGuiLoginPage.getDriver();
				LOGGER.info("webDriver " + driver);
			} catch (Exception e) {
				errorMessage += e.getMessage();
				LOGGER.error("Exception occurred during Gateway Admin Page login : " + errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Lan gui admin login successful");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 8 : Launch the IPv4 Firewall page from LAN GUI page
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Unable to navigate to Firewall page from the 'At a Glance' Menu";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Launch the IPv4 Firewall page from partner page");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Navigate to IPv4 Firewall page by clicking on \"Firewall\" Menu and Click on \"IPv4\"");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : IPv4 Firewall page should be displayed having page title as \"Gateway > Firewall> IPv4\"");
			LOGGER.info("**********************************************************************************");
			try {
				if (BroadBandCommonPage.navigateToFirewall(driver, device)) {
					errorMessage = "Unable to navigate to Firewall IPv4 page from the 'Firewall' Menu";
					status = LanSideBasePage.isFireWallPageLaunchedForPartners(device, tapEnv,
							BroadBandWebGuiTestConstant.LINK_TEXT_IPV4, BroadBandTestConstants.FIREWALL_IPV4);
				}
			} catch (Exception e) {
				LOGGER.error("Exception occured while navigating to Firewall IPV4 Page :" + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : IPv4 Firewall page is displayed having page title as 'Gateway > Firewall> IPv4'");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 9 : Verify the Default IPv4 firewall mode is set to Minimum Security
			 * (Low)
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Default firewall mode for IPv4 retrieved from Web GUI is not 'Minimum Security (Low)'";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify the Default IPv4 firewall mode is set to Minimum Security (Low)");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Retrieve the firewall level selected in \"Gateway > Firewall> IPv4\" Page");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : Default firewall mode should be Minimum Security (Low)");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
					BroadBandTestConstants.DEFAULT_IPV4_FIREWALL_SECURITY, BroadBandCommonPage
							.getFirewallSecurityLevel(driver, device, BroadBandTestConstants.String_CONSTANT_IPV4));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Default firewall mode for IPv4 retrieved from Web GUI is 'Minimum Security (Low)'");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

			/**
			 * Step 10 : Launch the IPv6 FireWall page from partner page
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Unable to navigate to Firewall IPv6 page from the 'Firewall' Menu";
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Launch the IPv6 FireWall page from partner page");
				LOGGER.info(
						"STEP " + stepNumber + " : ACTION : Navigate to IPv6 Firewall page by clicking on \"IPv6\"");
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : IPv6 Firewall page should be displayed having page title as \"Gateway > Firewall> IPv6\"");
				LOGGER.info("**********************************************************************************");
				status = LanSideBasePage.isFireWallPageLaunchedForPartners(device, tapEnv,
						BroadBandWebGuiTestConstant.LINK_TEXT_IPV6, BroadBandTestConstants.FIREWALL_IPV6);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : IPv6 Firewall page is displayed having page title as 'Gateway > Firewall> IPv6'");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum,
						status, errorMessage, true);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step...");
				tapEnv.updateExecutionForAllStatus(device, testId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * Step 11 : Verify the Default IPv6 firewall mode is set to Typical Security
			 * (default)
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				errorMessage = "Default firewall mode for IPv6 retrieved from Web GUI is not 'Typical Security (default)'";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify the Default IPv6 firewall mode is set to Typical Security (default)");
				LOGGER.info("STEP " + stepNumber
						+ " : ACTION : Retrieve the firewall level selected in \"Gateway > Firewall> IPv6\" Page");
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Default firewall mode should be Typical Security (default)");
				LOGGER.info("**********************************************************************************");
				status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
						BroadBandTestConstants.DEFAULT_IPV6_FIREWALL_SECURITY, BroadBandCommonPage
								.getFirewallSecurityLevel(driver, device, BroadBandTestConstants.String_CONSTANT_IPV6));
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Default firewall mode for IPv4 retrieved from Web GUI is 'Typical Security (default)'");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum,
						status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step...");
				tapEnv.updateExecutionForAllStatus(device, testId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * Step 12 : Launch the Managed Devices under Parental Control page from partner
			 * page
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Unable to navigate to Managed Devices page from the 'At a Glance' Menu";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Launch the Managed Devices under Parental Control page from partner page");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Navigate to Managed Devices page by clicking on \"Parental Control\" Menu and select the submenu \"Managed Devices\" .");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Managed Devices page should be displayed having page title as \"Parental control > Managed Devices\"");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonPage.navigateToManagedDevicesFromAtAGlance(driver, device);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully navigated to Managed Devices page and verified page title");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 13 : Verify the default configurations in Managed devices page
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Unable to retrieve Managed Devices status from Web GUI";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify the default configurations in Managed devices page ");
			LOGGER.info("STEP " + stepNumber + " : ACTION : The Managed Devices should be in disable state");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : By default, Managed Devices should be in Disable state");
			LOGGER.info("**********************************************************************************");
			try {
				status = LanSideBasePage.isSelected(
						By.xpath(BroadBandWebGuiElements.XPATH_MANAGED_DEVICES_STATUS_DISABLED_RADIO_BUTTON));
				LOGGER.info("Is Managed Devices disabled by default : " + status);
				if (!status) {
					boolean isEnabled = LanSideBasePage.isSelected(
							By.xpath(BroadBandWebGuiElements.XPATH_MANAGED_DEVICES_STATUS_ENABLED_RADIO_BUTTON));
					LOGGER.info("Is Managed Devices enabled : " + isEnabled);
					if (isEnabled) {
						errorMessage = "Managed Devices is Enabled by default after Factory Reset";
					}
				}
			} catch (Exception e) {
				LOGGER.error(
						"Exception occured while navigating to Managed Devices Page and Checking for Default Values:"
								+ e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL :  By default, Managed Devices is in Disable state");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.info("EXCEPTION OCCURRED WHILE VALIDATING testToVerifyAdminPageAccessUsingDefaultPassword():"
					+ e.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST CONDITION 1 : CLOSE THE LAN SIDE BROWSER.
			 */
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION " + postConStepNumber + " : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION " + postConStepNumber + " : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info(
					"POST-CONDITION " + postConStepNumber + " : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("##########################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION " + postConStepNumber + " : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION " + postConStepNumber + " : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			/**
			 * POST-CONDITION 2 : REACTIVATE THE GATEWAY DEVICE
			 */
			postConStepNumber++;
			if (isFactoryReset && !isReactivated) {
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, isReactivated,
						postConStepNumber);
			} else {
				LOGGER.info("Skipping Post Condition 2, since device is already reactivated");
			}

			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-DFLT-ADM-LOGIN-5001");
	}

	/**
	 *
	 * Test Case : Verify the user admin page can be logged in using cusadmin
	 * credentials in the wired and wireless connected clients and Validate negative
	 * Scenario in Admin GUI Login
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify 2.4 GHz SSID is enabled.</li>
	 * <li>PRE-CONDITION 2 : Verify 5 GHz SSID is enabled.</li>
	 * <li>PRE-CONDITION 3 : Verify the private wifi 2.4 GHz and 5 GHz ssid's are
	 * broadcasting in connected client.</li>
	 * <li>Step 1 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>Step 2 : Verify 2.4 GHz connected client's mac address in gateway</li>
	 * <li>Step 3 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 4 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 5 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 6 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 7 : Verify the gateway admin page is accessible in 2.4 GHz Wi-Fi
	 * connected client and Try Entering Username as "admin" and Password : (a Wrong
	 * Password)</li>
	 * <li>Step 8 : Verify the gateway admin page is accessible in 2.4 GHz Wi-Fi
	 * connected client and Try Entering Username as "admin" and Password : ""(Empty
	 * Password)</li>
	 * <li>Step 9 : Verify the gateway admin page is accessible in 2.4 GHz Wi-Fi
	 * connected client and can be logged in using admin/password credentials for
	 * Residential and cusadmin/highspeed for Commercial devices</li>
	 * <li>Step 10 : Verify disconnecting the 2.4 GHz SSID</li>
	 * <li>Step 11 : Connect the client to 5 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>Step 12 : Verify 5 ghz connected client's mac address in gateway</li>
	 * <li>Step 13 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 14 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 15 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 16 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 17 : Verify the gateway admin page is accessible in 5 GHz Wi-Fi
	 * connected client and Try Entering Username as "admin" and Password : (a Wrong
	 * Password)</li>
	 * <li>Step 18 : Verify the gateway admin page is accessible in 5 GHz Wi-Fi
	 * connected client and Try Entering Username as "admin" and Password : ""(Empty
	 * Password)</li>
	 * <li>Step 19 : Verify the gateway admin page is accessible in 5 GHz Wi-Fi
	 * connected client and can be logged in using admin/password credentials for
	 * Residential and cusadmin/highspeed for Commercial devices</li>
	 * <li>Step 20 : Verify disconnecting the 5 GHz SSID</li>
	 * <li>Step 21 : verify the ethernet client is connected</li>
	 * <li>Step 22 : Verify the IPv4 Address is retrieved from the client connected
	 * with Ethernet</li>
	 * <li>Step 23 : Verify the IPv6 Address is retrieved from the client connected
	 * with Ethernet</li>
	 * <li>Step 24 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with ethernet</li>
	 * <li>Step 25 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with ethernet</li>
	 * <li>Step 26 : Verify the gateway admin page is accessible in Ethernet
	 * connected client and Try Entering Username as "admin" and Password : (a Wrong
	 * Password)</li>
	 * <li>Step 27 : Verify the gateway admin page is accessible in Ethernet
	 * connected client and Try Entering Username as "admin" and Password : ""(Empty
	 * Password)</li>
	 * <li>Step 28 : Verify the gateway admin page is accessible in Ethernet
	 * connected client and can be logged in using admin/password credentials for
	 * Residential and cusadmin/highspeed for Commercial devices</li>
	 * <li>POST-CONDITION 1 : Close the lan browser.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Athira
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI, BroadBandTestGroup.WEBGUI })
	@TestDetails(testUID = "TC-RDKB-ADMIN-PAGE-5001")
	public void testToVerifyUserAdminInWiredAndWirelessClients(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-ADMIN-PAGE-501";
		String errorMessage = null;
		boolean status = false;
		int stepNumber = 1;
		int postConStepNumber = 1;
		int preConStepNumber = 1;
		String step = "S" + stepNumber;
		List<Dut> ssidVisibleDevices = new ArrayList<Dut>();
		Dut ssidVisibleDeviceOne = null;
		Dut deviceConnectedWithEthernet = null;
		boolean isLoggedIn = false;
		boolean isAdminPageLaunched = false;
		// Variable Declaration ends
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-ADMIN-PAGE-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the user admin page can be logged in using cusadmin credentials in the wired and wireless connected clients and Validate negative Scenario in Admin GUI Login");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(" PRE CONDITION 1 : Verify 2.4 GHz SSID is enabled.");
			LOGGER.info(" PRE CONDITION 2 : Verify 5 GHz SSID is enabled.");
			LOGGER.info(
					" PRE CONDITION 3 : Verify the private wifi 2.4 GHz and 5 GHz ssid's are broadcasting in connected client.");
			LOGGER.info("Step 1 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify connection status");
			LOGGER.info("Step 2 : Verify 2.4 GHz connected client's mac address in gateway");
			LOGGER.info("Step 3 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID");
			LOGGER.info("Step 4 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID");
			LOGGER.info(
					"Step 5 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz");
			LOGGER.info(
					"Step 6 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz");
			LOGGER.info(
					"Step 7 : Verify the gateway admin page is accessible in 2.4 GHz Wi-Fi connected client and Try Entering Username as 'admin' and Password : (a Wrong Password)");
			LOGGER.info(
					"Step 8 : Verify the gateway admin page is accessible in 2.4 GHz Wi-Fi connected client and Try Entering Username as 'admin' and Password : ''(Empty Password)");
			LOGGER.info(
					"Step 9 : Verify the gateway admin page is accessible in 2.4 GHz Wi-Fi connected client and can be logged in using admin/****** credentials for Residential and cusadmin/****** for Commercial devices");
			LOGGER.info("Step 10 : Verify disconnecting the 5 GHz SSID");
			LOGGER.info("Step 11 : Connect the client to 5 GHz Private Wi-Fi Network and verify connection status");
			LOGGER.info("Step 12 : Verify 5 GHz connected client's mac address in gateway");
			LOGGER.info("Step 13 : Verify the correct IPv4 address for client connected with 5 GHz SSID");
			LOGGER.info("Step 14 : Verify the correct IPv6 address for client connected with 5 GHz SSID");
			LOGGER.info(
					"Step 15 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz");
			LOGGER.info(
					"Step 16 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz");
			LOGGER.info(
					"Step 17 : Verify the gateway admin page is accessible in 5 GHz Wi-Fi connected client and Try Entering Username as 'admin' and Password : (a Wrong Password)");
			LOGGER.info(
					"Step 18 : Verify the gateway admin page is accessible in 5 GHz Wi-Fi connected client and Try Entering Username as 'admin' and Password : ''(Empty Password)");
			LOGGER.info(
					"Step 19 : Verify the gateway admin page is accessible in 5 GHz Wi-Fi connected client and can be logged in using admin/****** credentials for Residential and cusadmin/****** for Commercial devices");
			LOGGER.info("Step 20 : Verify disconnecting the 5 GHz SSID");
			LOGGER.info("Step 21 : verify the ethernet client is connected");
			LOGGER.info("Step 22 : Verify the IPv4 Address is retrieved from the client connected with Ethernet");
			LOGGER.info("Step 23 : Verify the IPv6 Address is retrieved from the client connected with Ethernet");
			LOGGER.info(
					"Step 24 : Verify whether have connectivity using that particular interface using IPV4 for client connected with ethernet");
			LOGGER.info(
					"Step 25 : Verify whether have connectivity using that particular interface using IPV6 for client connected with ethernet");
			LOGGER.info(
					"Step 26 : Verify the gateway admin page is accessible in Ethernet connected client and Try Entering Username as 'admin' and Password : 'comcast12'(Wrong Password)");
			LOGGER.info(
					"Step 27 : Verify the gateway admin page is accessible in Ethernet connected client and Try Entering Username as 'admin' and Password : ''(Empty Password)");
			LOGGER.info(
					"Step 28 : Verify the gateway admin page is accessible in Ethernet connected client and can be logged in using admin/****** credentials for Residential and cusadmin/****** for Commercial devices");
			LOGGER.info(" POST CONDITION 1 : Close the lan browser.");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			/**
			 * PRE-CONDITION 1-2 : VERIFY THE PRIVATE WIFI 2.4 GHz AND 5 GHz SSID'S ARE
			 * ENABLED
			 */
			BroadBandPreConditionUtils.executePreConditionToVerifyRadioStatus(device, tapEnv, preConStepNumber);
			/**
			 * PRE-CONDITION 3 : VERIFY THE PRIVATE WIFI 2.4 GHz AND 5 GHz SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber = 3;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_1);
			ssidVisibleDeviceOne = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			/**
			 * Step 1 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * STEP 2 : VERIFY 2.4 GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyClientMacInGateway(device, testCaseId, ssidVisibleDeviceOne,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ, stepNumber);

			/**
			 * SETP 3-6 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED WITH
			 * 2.4GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);
			/**
			 * SETP 7 : VALIDATE ADMIN GUI LOGIN WITH WRONG PASSWORD.
			 */
			stepNumber = 7;
			isAdminPageLaunched = executeTestStepToVerifyNegativeScenario(device, tapEnv, testCaseId, stepNumber,
					ssidVisibleDeviceOne, BroadBandTestConstants.PASSWORD_MISMATCH, false);

			/**
			 * SETP 8 : VALIDATE ADMIN GUI LOGIN WITH NO PASSWORD.
			 */
			stepNumber++;
			executeTestStepToVerifyNegativeScenario(device, tapEnv, testCaseId, stepNumber, ssidVisibleDeviceOne,
					BroadBandTestConstants.EMPTY_STRING, isAdminPageLaunched);

			/**
			 * STEP 9 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING USERID AND
			 * PASSWORD
			 */
			stepNumber++;
			isLoggedIn = LanSidePageNavigation.executeTestStepToLoginLanAdminWebGui(device, tapEnv,
					ssidVisibleDeviceOne, testCaseId, stepNumber);

			/**
			 * Step 10 : DISCONNECT THE CLIENT CONNECTED WITH 2.4GHz SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 11 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * STEP 12 : VERIFY 5 GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyClientMacInGateway(device, testCaseId, ssidVisibleDeviceOne,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ, stepNumber);

			/**
			 * SETP 13-16 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);
			/**
			 * SETP 17 : VALIDATE ADMIN GUI LOGIN WITH WRONG PASSWORD.
			 */
			stepNumber = 17;
			isAdminPageLaunched = executeTestStepToVerifyNegativeScenario(device, tapEnv, testCaseId, stepNumber,
					ssidVisibleDeviceOne, BroadBandTestConstants.PASSWORD_MISMATCH, false);

			/**
			 * SETP 18 : VALIDATE ADMIN GUI LOGIN WITH NO PASSWORD.
			 */
			stepNumber++;
			executeTestStepToVerifyNegativeScenario(device, tapEnv, testCaseId, stepNumber, ssidVisibleDeviceOne,
					BroadBandTestConstants.EMPTY_STRING, isAdminPageLaunched);

			/**
			 * STEP 19 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING USERID AND
			 * PASSWORD
			 */
			stepNumber++;
			isLoggedIn = LanSidePageNavigation.executeTestStepToLoginLanAdminWebGui(device, tapEnv,
					ssidVisibleDeviceOne, testCaseId, stepNumber);

			/**
			 * Step 20 : DISCONNECT THE CLIENT CONNECTED WITH 5 GHZ SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * STEP 21 : OBTAIN A ETHERNET CLIENT ASSOSIATED WITH THE SETTOP
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP :  " + stepNumber + " : DESCRIPTION : Obtain a ethernet client assosiated with the device");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : Connect a ethernet client assosiated with the device");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: The connection must be successful");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to obtain a ethernet client assosiated with the device";
			try {
				deviceConnectedWithEthernet = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWithEthernet != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL: Successfully connected a ethernet client assosiated with the device ");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

			/**
			 * SETP 22-25 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH ETHERNET AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, deviceConnectedWithEthernet,
					stepNumber);

			/**
			 * SETP 26 : VALIDATE ADMIN GUI LOGIN WITH WRONG PASSWORD.
			 */
			stepNumber = 26;
			isAdminPageLaunched = executeTestStepToVerifyNegativeScenario(device, tapEnv, testCaseId, stepNumber,
					deviceConnectedWithEthernet, BroadBandTestConstants.PASSWORD_MISMATCH, false);

			/**
			 * SETP 27 : VALIDATE ADMIN GUI LOGIN WITH NO PASSWORD.
			 */
			stepNumber++;
			executeTestStepToVerifyNegativeScenario(device, tapEnv, testCaseId, stepNumber, deviceConnectedWithEthernet,
					BroadBandTestConstants.EMPTY_STRING, isAdminPageLaunched);

			/**
			 * STEP 28 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING USERID AND
			 * PASSWORD
			 */
			stepNumber++;
			isLoggedIn = LanSidePageNavigation.executeTestStepToLoginLanAdminWebGui(device, tapEnv,
					deviceConnectedWithEthernet, testCaseId, stepNumber);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING ADMIN GUI LOGIN NEGATIVE SCENARIO : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			/**
			 * POST-CONDITION 1 : CLOSE THE LAN BROWSER
			 */
			BroadBandPostConditionUtils.postConditionCloseBrowser(isLoggedIn, postConStepNumber);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-ADMIN-PAGE-5001");
	}

	/**
	 * Test step method used to verify Negative Scenario of Admin GUI Login
	 * 
	 * @param device            instance of{@link Dut}
	 * @param tapEnv            instance of {@link AutomaticsTapApi}
	 * @param testCaseId        Test case ID
	 * @param stepNumber        Step Number
	 * @param deviceConnected   instance of{@link Dut}
	 * @param wrongPassword     Wrong password to validate
	 * @param adminPageLaunched Admin Page launched status
	 * 
	 * @refactor Athira
	 */
	public static boolean executeTestStepToVerifyNegativeScenario(Dut device, AutomaticsTapApi tapEnv,
			String testCaseId, int stepNumber, Dut deviceConnected, String wrongPassword, boolean adminPageLaunched) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		WebDriver driver = null;
		/**
		 * STEP : VERIFY LANGUI ADMIN PAGE LOGIN WITH NEGATIVE SCENARIO.
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : Verify the gateway admin page is accessible in connected client and Try Entering Username as 'admin' and Password : '"
				+ wrongPassword + "'");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : Retrive the Password and check if the Passowrd Field is retrieved as null "
				+ (CommonMethods.isNotNull(wrongPassword) ? "instead of the Wrong Password Entered is displayed"
						: "as value"));
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : Password Field Retrieved should be with null "
				+ (CommonMethods.isNotNull(wrongPassword) ? "instead of the Wrong Password Entered" : "as value"));
		LOGGER.info("**********************************************************************************");
		errorMessage = "Failed to verify negative scenario login.";
		try {
			status = LanWebGuiLoginPage.launchWebguipageAndValidateNegativeScenario(tapEnv, device, deviceConnected,
					wrongPassword, adminPageLaunched);
			driver = LanWebGuiLoginPage.getDriver();
			// Setting admin page launched status as true.
			adminPageLaunched = true;
		} catch (Exception e) {
			status = false;
			errorMessage += ". Exception while retrieving the password: " + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : Successfully verified negative scenario of Admin page login ");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		return adminPageLaunched;
	}

	/**
	 *
	 * Test Case : Validate the DHCP configuration Using SNMP
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 2.4/5 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>PRE-CONDITION 6 : Get the default dhcp ipv4 values</li>
	 * <li>PRE-CONDITION 7 : Verify partner Id is Comcast,If not change it to
	 * comcast</li>
	 * <li>Step 1 : Set and verify the subnet mask value using SNMP</li>
	 * <li>Step 2 : Set and verify the dhcp ipv4 begining address using SNMP</li>
	 * <li>Step 3 : Set and verify the dhcp ipv4 ending address using SNMP</li>
	 * <li>Step 4 : Set and verify the dhcp ipv4 lease time using SNMP</li>
	 * <li>Step 5 : Verify the gateway admin page is accessible in connected client
	 * and can be logged in using admin/****** credential for residential or
	 * cusadmin/****** credential for commercial devices</li>
	 * <li>Step 6 : Navigate to the GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION
	 * Page and verify navigation status</li>
	 * <li>Step 7 : Verify Retrieving the Subnet Mask value from the LanUI page and
	 * cross verify with SNMP value retrieved.</li>
	 * <li>Step 8 : Verify Retrieving the DHCPV4 Beginning value from the LanUI page
	 * and cross verify with SNMP value retrieved.</li>
	 * <li>Step 9 : Verify Retrieving the DHCPV4 Ending value from the LanUI page
	 * and cross verify with SNMP value retrieved.</li>
	 * <li>Step 10 : Verify Retrieving the DHCPV4 Lease Time value from the LanUI
	 * page and cross verify with SNMP value retrieved.</li>
	 * <li>Step 11 : Verify the correct ipv4 address for connected client
	 * device</li>
	 * <li>Step 12 : Verify the internet connectivity in the client connected using
	 * IPV4</li>
	 * <li>POST-CONDITION 1 : Verify lan side browser closed</li>
	 * <li>POST-CONDITION 2 : Set the default dhcp ipv4 values</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Athira
	 *
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-DHCP-SNMP-5001")
	public void testToSetAndVerifyDhcpConfigurationUsignSnmpInWebUi(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-DHCP-SNMP-501";
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		String errorMessage = "";
		boolean status = false;
		WebDriver driver = null;
		BroadbandLocalIpConfigurationPage localIpPage = null;
		Dut deviceConnected = null;
		HashMap<String, String> defaultDchpIpv4ValuesMap = null;
		isBrowserOpen = false;
		String subnetMaskValue = null;
		String dhcpIpv4StartIp = null;
		String dhcpIpv4EndIp = null;
		String dhcpIpv4LeaseTime = null;
		String snmpResponse = null;
		String webPaResponse = null;
		BroadBandResultObject broadBandResultObject = null;
		boolean isBusinessClassDevice = false;
		// Variable Declation Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-DHCP-SNMP-5001");
		LOGGER.info("TEST DESCRIPTION: Validate the DHCP configuration Using SNMP");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 2.4/5 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with 2.4/5 GHz SSID");
		LOGGER.info("PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with 2.4/5 GHz SSID");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
		LOGGER.info("PRE-CONDITION 6 : Get the default dhcp ipv4 values");

		LOGGER.info("Step 1 : Set and verify the subnet mask value using SNMP");
		LOGGER.info("Step 2 : Set and verify the dhcp ipv4 begining address using SNMP");
		LOGGER.info("Step 3 : Set and verify the dhcp ipv4 ending address using SNMP");
		LOGGER.info("Step 4 : Set and verify the dhcp ipv4 lease time using SNMP");
		LOGGER.info(
				"Step 5 : Verify the gateway admin page is accessible in connected client and can be logged in using admin/****** credential for residential or cusadmin/****** credential for commercial devices");
		LOGGER.info(
				"Step 6 : Navigate to the GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION Page and verify navigation status");
		LOGGER.info(
				"Step 7 : Verify Retrieving the Subnet Mask value from the LanUI page and cross verify with SNMP value retrieved.");
		LOGGER.info(
				"Step 8 : Verify Retrieving the DHCPV4 Beginning value from the LanUI page and cross verify with SNMP value retrieved.");
		LOGGER.info(
				"Step 9 : Verify Retrieving the DHCPV4 Ending value from the LanUI page and cross verify with SNMP value retrieved.");
		LOGGER.info(
				"Step 10 : Verify Retrieving the DHCPV4 Lease Time value from the LanUI page and cross verify with SNMP value retrieved.");
		LOGGER.info("Step 11 : Verify the correct ipv4 address for connected client device");
		LOGGER.info("Step 12 : Verify the internet connectivity in the client connected using IPV4");
		LOGGER.info("POST-CONDITION 1 : Verify lan side browser closed");
		LOGGER.info("POST-CONDITION 2 : Set the default dhcp ipv4 values");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRECONDITION 1-5 :
			 */
			deviceConnected = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
					BroadBandTestConstants.BAND_2_4GHZ_OR_5GHZ);

			/**
			 * PRECONDITION 6 : GET THE DEFAULT DHCP IPV4 VALUES
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 6 : DESCRIPTION : GET THE DEFAULT DHCP IPV4 VALUES");
			LOGGER.info("PRE-CONDITION 6 : ACTION : GET THE DEFAULT DHCP IPV4 VALUES USING WEBPA");
			LOGGER.info("PRE-CONDITION 6 : EXPECTED : MUST RETRIEVE THE DEFAULT DHCP IPV4 VALUES ");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET DEFAULT DHCP IPV4 VALUES";
			defaultDchpIpv4ValuesMap = BroadBandPreConditionUtils
					.executePreconditionToGetTheDefaultDchpIpv4Values(device, tapEnv);
			status = (null != defaultDchpIpv4ValuesMap);
			if (status) {
				LOGGER.info("PRE-CONDITION 6 : ACTUAL : DEFAULT DHCP IPV4 VALUES ARE RETRIEVED SUCCESSFULLY");
			} else {
				LOGGER.error("PRE-CONDITION 6 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 6 : FAILED : " + errorMessage);
			}

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : SET AND VERIFY THE SUBNET MASK VALUE USING SNMP
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": Set and verify the subnet mask value using SNMP");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute SNMP set command using mib 1.3.6.1.4.1.17270.50.2.3.2.1.5.32 a <SubnetMaskValue>");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Must set and verify the subnet mask value using SNMP");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set and verify the subnet mask value using SNMP";
			isBusinessClassDevice = DeviceModeHandler.isBusinessClassDevice(device);
			subnetMaskValue = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTestConstants.STRING_VALUE_TWO_HUNDRED_AND_FIFTY_FIVE, BroadBandTestConstants.DOT_OPERATOR,
					BroadBandTestConstants.STRING_VALUE_TWO_HUNDRED_AND_FIFTY_FIVE, BroadBandTestConstants.DOT_OPERATOR,
					String.valueOf(BroadBandTestConstants.CONSTANT_0), BroadBandTestConstants.DOT_OPERATOR,
					String.valueOf(BroadBandTestConstants.CONSTANT_0));
			snmpResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SUBNET_MASK.getOid(), SnmpDataType.STRING_A, subnetMaskValue,
					BroadBandSnmpMib.ECM_SUBNET_MASK.getTableIndex());
			webPaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_LAN_SUBNET);
			status = CommonMethods.isNotNull(snmpResponse) && CommonMethods.isIpv4Address(snmpResponse)
					&& CommonMethods.isNotNull(webPaResponse) && CommonMethods.isIpv4Address(webPaResponse)
					&& CommonMethods.patternMatcher(snmpResponse, webPaResponse)
					&& CommonMethods.patternMatcher(subnetMaskValue, webPaResponse);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully set and verified the subnet mask value using SNMP");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 2 : SET AND VERIFY THE DHCP IPV4 BEGINING ADDRESS USING SNMP
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": Set and verify the dhcp ipv4 begining address using SNMP");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute SNMP set command using mib 1.3.6.1.4.1.17270.50.2.3.3.1.2.32 a <DhcpIpv4_StartIp>");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Must set and verify the dhcp ipv4 begining address using SNMP");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set and verify the dhcp ipv4 begining address using SNMP";
			dhcpIpv4StartIp = BroadBandCommonUtils.concatStringUsingStringBuffer(
					String.valueOf(BroadBandTestConstants.CONSTANT_10), BroadBandTestConstants.DOT_OPERATOR,
					String.valueOf(isBusinessClassDevice ? BroadBandTestConstants.CONSTANT_1
							: BroadBandTestConstants.CONSTANT_0),
					BroadBandTestConstants.DOT_OPERATOR, String.valueOf(BroadBandTestConstants.CONSTANT_15),
					BroadBandTestConstants.DOT_OPERATOR, String.valueOf(BroadBandTestConstants.CONSTANT_20));
			snmpResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_DHCP_START_IP.getOid(), SnmpDataType.STRING_A, dhcpIpv4StartIp,
					BroadBandSnmpMib.ECM_DHCP_START_IP.getTableIndex());
			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_MINADDRESS);
			status = CommonMethods.isNotNull(snmpResponse) && CommonMethods.isIpv4Address(snmpResponse)
					&& CommonMethods.isNotNull(webPaResponse) && CommonMethods.isIpv4Address(webPaResponse)
					&& CommonMethods.patternMatcher(snmpResponse, webPaResponse)
					&& CommonMethods.patternMatcher(dhcpIpv4StartIp, webPaResponse);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully set and verify the dhcp ipv4 begining address using SNMP");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 3 : SET AND VERIFY THE DHCP IPV4 ENDING ADDRESS USING SNMP
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": Set and verify the dhcp ipv4 ending address using SNMP");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute SNMP set command using mib 1.3.6.1.4.1.17270.50.2.3.3.1.4.32 a <DhcpIpv4_EndIp>");
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : Must set and verify the dhcp ipv4 ending address using SNMP");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set and verify the dhcp ipv4 ending address using SNMP";
			dhcpIpv4EndIp = BroadBandCommonUtils.concatStringUsingStringBuffer(
					String.valueOf(BroadBandTestConstants.CONSTANT_10), BroadBandTestConstants.DOT_OPERATOR,
					String.valueOf(isBusinessClassDevice ? BroadBandTestConstants.CONSTANT_1
							: BroadBandTestConstants.CONSTANT_0),
					BroadBandTestConstants.DOT_OPERATOR, String.valueOf(BroadBandTestConstants.CONSTANT_50),
					BroadBandTestConstants.DOT_OPERATOR, String.valueOf(BroadBandTestConstants.CONSTANT_60));
			snmpResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_DHCP_END_IP.getOid(), SnmpDataType.STRING_A, dhcpIpv4EndIp,
					BroadBandSnmpMib.ECM_DHCP_END_IP.getTableIndex());
			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_MAXADDRESS);
			status = CommonMethods.isNotNull(snmpResponse) && CommonMethods.isIpv4Address(snmpResponse)
					&& CommonMethods.isNotNull(webPaResponse) && CommonMethods.isIpv4Address(webPaResponse)
					&& CommonMethods.patternMatcher(snmpResponse, webPaResponse)
					&& CommonMethods.patternMatcher(dhcpIpv4EndIp, webPaResponse);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully set and verify the dhcp ipv4 ending address using SNMP");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 4 : SET AND VERIFY THE DHCP IPV4 LEASE TIME USING SNMP
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": Set and verify the dhcp ipv4 lease time using SNMP");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute SNMP set command using mib 1.3.6.1.4.1.17270.50.2.3.3.1.5.32 u <DhcpIpv4_LeaseTime>");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Must set and verify the dhcp ipv4 lease time using SNMP");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set and verify the dhcp ipv4 lease time using SNMP";
			dhcpIpv4LeaseTime = BroadBandTestConstants.STRING_LEASE_TIME_VALUE;
			snmpResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_DHCP_LEASE_TIME.getOid(), SnmpDataType.UNSIGNED_INTEGER, dhcpIpv4LeaseTime,
					BroadBandSnmpMib.ECM_DHCP_LEASE_TIME.getTableIndex());
			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME);
			status = CommonMethods.isNotNull(snmpResponse) && CommonMethods.isNotNull(webPaResponse)
					&& CommonMethods.patternMatcher(snmpResponse, webPaResponse)
					&& CommonMethods.patternMatcher(dhcpIpv4LeaseTime, webPaResponse);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully set and verify the dhcp ipv4 lease time using SNMP");
				LOGGER.info("WAITING FOR THREE MINUTES  ");
				tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 5 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING VALID USERID AND
			 * VALID PASSWORD
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify the gateway admin page is accessible in connected client and can be logged in using admin/****** credential for residential or cusadmin/****** credential for commercial devices");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Launch the gateway admin gui in browser, once the page is loaded ,use username and password as admin/****** for residential or cusadmin/****** for commercial devices to login");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Gateway admin page should be accessible from client and can be able to login using admin/****** credential for residential or cusadmin/****** credential for commercial devices");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to login admin page in connected client";
			try {
				status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
				isBrowserOpen = status;
				driver = LanWebGuiLoginPage.getDriver();
				LOGGER.info("webDriver " + driver);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error("Exception occurred during Gateway Admin Page login : " + errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Lan gui admin login successful");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 6 : NAVIGATE TO THE GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION PAGE
			 * AND VERIFY NAVIGATION STATUS
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Navigate to the GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION Page and verify navigation status");
			LOGGER.info("STEP " + stepNumber + " : ACTION : Click on GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Navigation should be successful and it should display the GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION Page");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to verify navigation status on GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION Page";
			try {
				localIpPage = new BroadbandLocalIpConfigurationPage(driver);
				status = localIpPage.navigateToLocalIpPage(tapEnv, device);
			} catch (Exception exception) {
				LOGGER.error("Exception in launching LOCAL IP CONFIGURATION Page " + exception.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Navigation successful for GATEWAY > CONNECTIONS > LOCAL IP CONFIGURATION Page");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 7 : GET AND VERIFY THE SUBNET MASK VALUE IN LOCAL IP CONFIGURATION PAGE
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the Subnet Mask value from the LanUI page and cross verify with SNMP value retrieved.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Retrieve the Subnet Mask Address from LanUI page and get the SNMP value using the parameter");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Subnet Mask Value retrieved from LanUI and value retrieved from SNMP should be same.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Subnet Mask Address value retrieved from LanUI doesn't match with the value retrieved from SNMP .";
			status = BroadBandWebUiUtils.verifySubnetMaskAddressFromLocalIpNetworkPageAndSnmp(driver, tapEnv, device,
					isBusinessClassDevice);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Subnet Mask Value retrieved from Local IP Network Page in LanUI and value retrieved from SNMP is same as expected.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 8 : GET AND VERIFY THE DHCP IPV4 BEGINING ADDRESS IN LOCAL IP
			 * CONFIGURATION PAGE
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the DHCPV4 Beginning value from the LanUI page and cross verify with SNMP value retrieved.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Retrieve the  DHCPV4 Beginning value from LanUI page and get the SNMP value using the parameter");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  DHCPV4 Beginning value retrieved from LanUI and value retrieved from SNMP should be same.");
			LOGGER.info("**********************************************************************************");
			errorMessage = " DHCPV4 Beginning value retrieved from LanUI doesn't match with the value retrieved from SNMP .";
			status = BroadBandWebUiUtils.verifyDhcpBeginningAddressFromLocalIpNetworkPageAndSnmp(driver, tapEnv,
					device);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : DHCPV4 Beginning value retrieved from Local IP Network Page in LanUI and value retrieved from SNMP is same as expected.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

			/**
			 * Step 9 : GET AND VERIFY THE DHCP IPV4 ENDING ADDRESS IN LOCAL IP
			 * CONFIGURATION PAGE
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the DHCPV4 Ending value from the LanUI page and cross verify with SNMP value retrieved.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Retrieve the  DHCPV4 Ending value from LanUI page and get the SNMP value using the parameter");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  DHCPV4 Ending value retrieved from LanUI and value retrieved from SNMP should be same.");
			LOGGER.info("**********************************************************************************");
			errorMessage = " DHCPV4 Ending value retrieved from LanUI doesn't match with the value retrieved from SNMP .";
			status = BroadBandWebUiUtils.verifyDhcpEndingAddressFromLocalIpNetworkPageAndSnmp(driver, tapEnv, device);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : DHCPV4 Ending value retrieved from Local IP Network Page in LanUI and value retrieved from SNMP is same as expected.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

			/**
			 * Step 10 : GET AND VERIFY THE DHCP IPV4 LEASE TIME IN LOCAL IP CONFIGURATION
			 * PAGE
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP" + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the DHCPV4 Lease Time value from the LanUI page and cross verify with SNMP value retrieved.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Retrieve the  DHCPV4 Lease Time value from LanUI page and get the SNMP value using the parameter");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  DHCPV4 Lease Time value retrieved from LanUI and value retrieved from SNMP should be same.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebUiUtils.verifyDhcpLeaseTimeFromLocalIpNetworkPageAndSnmp(driver, tapEnv, device);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Retrieved the dhcpv4 lease time from local ip network page successfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

			/**
			 * Step 11 : VERIFY IPV4 IS ASSIGNED ON THE CONNECTED CLIENT
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT DEVICE");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS WITH IN THE DHCP IPV4 BEGIN ADDRESS "
					+ dhcpIpv4StartIp + " AND ENDING ADDRESS " + dhcpIpv4EndIp + " RANGE");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO GET THE CORRECT IPV4 ADDRESS BETWEEN BEGIN ADDRESS " + dhcpIpv4StartIp
					+ " AND ENDING ADDRESS " + dhcpIpv4EndIp + " RANGE FROM CLIENT";
			BroadBandConnectedClientUtils.dhcpRenewInConnectedClient(deviceConnected, tapEnv);
			LOGGER.info("IP RENEW COMPLETD ");
			String ipv4AddressRetrievedFromClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
					device, deviceConnected);
			LOGGER.info("IP ADDRESS ASSIGNED TO THE CONNECTED CLIENT FROM DHCP : " + ipv4AddressRetrievedFromClient);
			status = BroadBandConnectedClientUtils.validateDhcpIpv4AddressBetweenRangeInConnectedClient(dhcpIpv4StartIp,
					dhcpIpv4EndIp, ipv4AddressRetrievedFromClient);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV4 ADDRESS BETWEEN BEGIN ADDRESS "
						+ dhcpIpv4StartIp + " AND ENDING ADDRESS " + dhcpIpv4EndIp + " RANGE FROM CLIENT");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 12 : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED USING IPV4
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify the internet connectivity in the client connected using IPV4");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : The internet connectivity must be available interface using IPV4 ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Not able to access the site 'www.google.com' from connected client with using IPV4 ";
			broadBandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
					tapEnv, deviceConnected,
					BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
					BroadBandTestConstants.IP_VERSION4);
			status = broadBandResultObject.isStatus();
			errorMessage = broadBandResultObject.getErrorMessage();
			if (!status) {
				errorMessage = "Pign operation failed to access the site 'www.google.com' USING IPV4 ";
				status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(deviceConnected, tapEnv,
						BroadBandTestConstants.PING_TO_GOOGLE, BroadBandTestConstants.IP_VERSION4);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified connected client has internet connectivity using IPV4 ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.info("EXCEPTION OCCURRED WHILE VALIDATING testToSetAndVerifyDhcpConfigurationUsignSnmp():"
					+ e.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST CONDITION 1 : CLOSE THE LAN SIDE BROWSER.
			 */
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("##########################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}

			/**
			 * POST-CONDITION 2 : SET THE DEFAULT DHCP IPV4 VALUES
			 */
			if (defaultDchpIpv4ValuesMap != null) {
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION 2 : DESCRIPTION : SET THE DEFAULT DHCP IPV4 VALUES");
				LOGGER.info("POST-CONDITION 2 : ACTION : SET THE DEFAULT DHCP IPV4 VALUES USING WEBPA");
				LOGGER.info("POST-CONDITION 2 : EXPECTED : MUST SET THE DEFAULT DHCP IPV4 VALUES ");
				LOGGER.info("#######################################################################################");
				errorMessage = "FAILED TO SET DEFAULT DHCP IPV4 VALUES";

				status = BroadBandPostConditionUtils.executePostConditionToSetTheDefaultDchpIpv4Values(device, tapEnv,
						defaultDchpIpv4ValuesMap);

				if (status) {
					LOGGER.info("POST-CONDITION 2 : ACTUAL : SUCCESSFULLY SET THE DEFAULT DHCP IPV4 VALUES.");
				} else {
					LOGGER.info("POST-CONDITION 2 : ACTUAL : " + errorMessage);
				}
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-DHCP-SNMP-5001");
	}

	/**
	 *
	 * Test Case : Verify the configuration of Transmission Control Rate for 5Ghz(n)
	 * Radio after reboot.
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 2.4/5 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>PRE-CONDITION 6 : Reactive the router device</li>
	 * <li>PRE-CONDITION 7 : Get the default operating standards</li>
	 * <li>Step 1 : Verify login into the LAN GUI Adimn page by using valid userid
	 * and valid password</li>
	 * <li>Step 2 : Navigate to the Gateway>Connection>WIFI and verify Navigation
	 * Status</li>
	 * <li>Step 3 : Navigate to the Gateway>Connection>WIFI >Edit 5 Ghz and verify
	 * Navigation Status</li>
	 * <li>Step 4 : Set the operation standard mode as n in LAN GUI Adimn page</li>
	 * <li>Step 5 : Verify the value of operational transmission rate of the 5 GHz
	 * with operating standard as n using webpa</li>
	 * <li>Step 6 : Verify the value of Basic data transmission rates of the 5GHz
	 * with operating standard as n using webpa</li>
	 * <li>Step 7 : Verify the value of Supported data transmission rates of the
	 * 5GHz with operating standard as n using webpa</li>
	 * <li>Step 8 : Verify the value of Operational data transmission Rates of the
	 * 5GHz with operating standard as n using webpa</li>
	 * <li>Step 9 : Navigate to the Troubleshooting>Reset/Restore Gateway page and
	 * verify Navigation Status</li>
	 * <li>Step 10 : Reset the device through LAN GUI Adimn page.</li>
	 * <li>Step 11 : Verify the value of operational transmission rate of the 5GHz
	 * with operating standard as n after reboot using webpa</li>
	 * <li>Step 12 : Verify the value of Basic data transmission rates of the 5GHz
	 * with operating standard as n after reboot with the value of step 6</li>
	 * <li>Step 13 : Verify the value of Supported data transmission rates of the
	 * 5GHz with operating standard as n after reboot with the value of step 7</li>
	 * <li>Step 14 : Verify the value of Operational data transmission Rates of the
	 * 5GHz with operating standard as n after reboot with the value of step 8</li>
	 * <li>POST-CONDITION 1 : Close the browser</li>
	 * <li>POST-CONDITION 2 : Set the default operating standards</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Rajapandian
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-RADIO-3002")
	public void testToVerifyTxRateConfigInWebUiForWifi2(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-RADIO-302";
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		String errorMessage = "";
		boolean status = false;
		WebDriver driver = null;
		LanSidePageNavigation lanSidePageNavigation = null;
		BroadBandCommonPage broadBandCommonPage = null;
		Dut deviceConnected = null;
		String basicDataTransmission5ghz = null;
		String supportedDataTransmission5ghz = null;
		String oparationalDataTransmission5ghz = null;
		String defaultOperatingStandard5ghz = null;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-RADIO-3002");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the configuration of Transmission Control Rate for 5Ghz(n) Radio after reboot.");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 2.4/5 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify  the correct IPv4  address for client connected with 2.4/5 GHz SSID");
		LOGGER.info("PRE-CONDITION 3 : Verify  the correct IPv6  address for client connected with 2.4/5 GHz SSID");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
		LOGGER.info("PRE-CONDITION 6 : Reactive the router device");
		LOGGER.info("PRE-CONDITION 7 : Get the default operating standards");
		LOGGER.info("Step 1. Verify login into the LAN GUI Adimn page by using valid userid and valid password");
		LOGGER.info("Step 2. Navigate to the Gateway>Connection>WIFI and verify Navigation Status");
		LOGGER.info("Step 3. Navigate to the Gateway>Connection>WIFI >Edit 5 Ghz and verify Navigation Status");
		LOGGER.info("Step 4. Set the operation standard mode as n in  LAN GUI Adimn page");
		LOGGER.info(
				"Step 5. Verify  the value of operational transmission rate of the 5 GHz with operating standard as n  using webpa");
		LOGGER.info(
				"Step 6. Verify  the value of Basic data transmission rates of the 5GHz with operating standard as n using webpa");
		LOGGER.info(
				"Step 7. Verify  the value of Supported data transmission rates of the 5GHz with operating standard as n using webpa");
		LOGGER.info(
				"Step 8. Verify  the value of Operational data transmission Rates of the 5GHz with operating standard as n using webpa");
		LOGGER.info("Step 9. Navigate to the Troubleshooting>Reset/Restore Gateway page and verify Navigation Status");
		LOGGER.info("Step 10. Reset the device through  LAN GUI Adimn page.");
		LOGGER.info(
				"Step 11. Verify the value of operational transmission rate of the 5GHz with operating standard as n after reboot using webpa.");
		LOGGER.info(
				"Step 12. Verify the value of Basic data transmission rates of the 5GHz with operating standard as n after reboot with the value of step 6");
		LOGGER.info(
				"Step 13. Verify the value of Supported data transmission rates of the 5GHz with operating standard as n after reboot with the value of step 7");
		LOGGER.info(
				"Step 14. Verify the value of Operational data transmission Rates of the 5GHz with operating standard as n after reboot with the value of step 8");
		LOGGER.info("POST-CONDITION 1 : Close the browser");
		LOGGER.info("POST-CONDITION 2 : Set the default operating standards");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRECONDITION 1-6 :
			 */
			deviceConnected = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
					BroadBandTestConstants.BAND_2_4GHZ_OR_5GHZ);
			BroadBandPreConditionUtils.executePreConditionToReacitivateDevice(device, tapEnv,
					BroadBandTestConstants.CONSTANT_6);
			/**
			 * PRECONDITION 7 : GET THE DEFAULT OPERATING STANDARDS.
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 7 : DESCRIPTION : GET THE DEFAULT OPERATING STANDARDS");
			LOGGER.info("PRE-CONDITION 7 : ACTION : GET THE DEFAULT OPERATING STANDARDS USING WEBPA");
			LOGGER.info("PRE-CONDITION 7 : EXPECTED : MUST RETRIEVE THE DEFAULT OPERATING STANDARDS VALUES");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET DEFAULT OPERATING STANDARDS";
			defaultOperatingStandard5ghz = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
			if (CommonMethods.isNotNull(defaultOperatingStandard5ghz)) {
				LOGGER.info("PRE-CONDITION 7 : ACTUAL: DEFAULT OPERATING STANDARDS ARE RETRIEVED SUCCESSFULLY.");
			} else {
				LOGGER.error("PRE-CONDITION 7 : ACTUAL: " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : 7 : FAILED : " + errorMessage);
			}

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING VALID USERID AND
			 * VALID PASSWORD
			 */
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : VERIFY THE GATEWAY ADMIN PAGE IS ACCESSIBLE IN CONNECTED CLIENT AND CAN BE LOGGED IN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : LAUNCH THE GATEWAY ADMIN GUI IN BROWSER URL : https://10.0.0.1 or https://10.1.10.1 , ONCE THE PAGE IS LOADED ,USE USERNAME AND PASSWORD AS ADMIN/****** FOR RESIDENTIAL OR CUSADMIN/****** FOR COMMERCIAL DEVICES TO LOGIN");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : GATEWAY ADMIN PAGE SHOULD BE ACCESSIBLE FROM CLIENT AND CAN BE ABLE TO LOGIN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO LOGIN GATEWAY ADMIN PAGE IN CONNECTED CLIENT";
			try {
				status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
				isBrowserOpen = status;
				driver = LanWebGuiLoginPage.getDriver();
				LOGGER.info(" webDriver " + driver);
				lanSidePageNavigation = new LanSidePageNavigation(driver);
				broadBandCommonPage = new BroadBandCommonPage(driver);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error("Exception occurred during Gateway Admin Page login : " + errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : LAN GUI ADMIN LOGIN SUCCESSFUL");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 2 : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI PAGE AND VERIFY
			 * NAVIGATION STATUS
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI PAGE AND VERIFY NAVIGATION STATUS");
			LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > WI-FI");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > WI-FI PAGE");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > WI-FI PAGE";
			LanSideWiFiPage lanSideWiFiPage = new LanSideWiFiPage(driver);
			try {
				if (lanSideWiFiPage.navigateToConnection(device, tapEnv)) {
					status = lanSideWiFiPage.navigateToWiFiPage(device, tapEnv);
				}
			} catch (Exception exception) {
				LOGGER.error("Exception in launching WiFi page " + exception.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > WI-FI PAGE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 3-4 : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI > EDIT 5 GHZ PAGE AND
			 * SET THE OPERATION STANDARD MODE AS n IN LAN GUI ADMIN PAGE.
			 */
			stepNumber++;
			executeTestStepToSetOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N, driver, lanSidePageNavigation,
					BroadBandTestConstants.WIRELESS_MODE_N, broadBandCommonPage);

			/**
			 * Step 5 : VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS n USING WEBPA.
			 */
			stepNumber = 5;
			executeTestStepToVerifyOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N, driver);

			/**
			 * Step 6 : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS n USING WEBPA.
			 */
			stepNumber++;
			basicDataTransmission5ghz = executeTestStepToVerifyBasicDataTxRates(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_N, driver);

			/**
			 * Step 7 : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS n USING WEBPA.
			 */
			stepNumber++;
			supportedDataTransmission5ghz = executeTestStepToVerifySupportedDataTxRates(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_N, driver);

			/**
			 * Step 8 : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES OF 5 GHZ
			 * WITH OPERATIONAL STANDARD AS n USING WEBPA.
			 */
			stepNumber++;
			oparationalDataTransmission5ghz = executeTestStepToVerifyOperationalDataTxRates(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_N, driver);
			BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv, deviceConnected,
					WEBPA_AP_INDEXES.PRIVATE_WIFI, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);

			/**
			 * Step 9-10 : RESET THE DEVICE THROUGH LAN GUI ADMIN PAGE.
			 */
			stepNumber++;
			executeTestStepToResetDevice(device, testCaseId, stepNumber, driver, lanSidePageNavigation);

			/**
			 * Step 11 : VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS n AFTER REBOOT USING WEBPA.
			 */
			stepNumber = 11;
			executeTestStepToVerifyOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N, driver);

			/**
			 * Step 12 : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS n AFTER REBOOT WITH THE VALUE OF STEP 6.
			 */
			stepNumber++;
			executeTestStepToVerifyBasicDataTxRatesAfterReboot(device, testCaseId, BroadBandTestConstants.BAND_5GHZ,
					stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_N, driver, basicDataTransmission5ghz);

			/**
			 * Step 13 : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS n AFTER REBOOT WITH THE VALUE OF STEP 7.
			 */
			stepNumber++;
			executeTestStepToVerifySupportedDataTxRatesAfterReboot(device, testCaseId, BroadBandTestConstants.BAND_5GHZ,
					stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_N, driver, supportedDataTransmission5ghz);

			/**
			 * Step 14 : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES OF 5 GHZ
			 * WITH OPERATIONAL STANDARD AS n AFTER REBOOT WITH THE VALUE OF STEP 8.
			 */
			stepNumber++;
			executeTestStepToVerifyOperationalDataTxRatesAfterReboot(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_N, driver,
					oparationalDataTransmission5ghz);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.info(
					"EXCEPTION OCCURRED WHILE VALIDATING testToVerifyTxRateConfigInWebUiForWifi():" + e.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST CONDITION 1 : CLOSE THE LAN SIDE BROWSER.
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("#######################################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			/**
			 * POST CONDITION 2 : SET THE DEFAULT OPERATING STANDARD.
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 2 : DESCRIPTION : SET THE DEFAULT OPERATING STANDARD");
			LOGGER.info("POST-CONDITION 2 : ACTION : SET THE DEFAULT OPERATING STANDARD USING WEBPA");
			LOGGER.info("POST-CONDITION 2 : EXPECTED : MUST SET THE DEFAULT OPERATING STANDARD ");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO SET DEFAULT OPERATING STANDARD";
			boolean isdefaultOperStd5ghz = false;
			if (CommonMethods.isNotNull(defaultOperatingStandard5ghz)) {
				isdefaultOperStd5ghz = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						BroadBandTestConstants.CONSTANT_0, defaultOperatingStandard5ghz,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			}
			if (isdefaultOperStd5ghz) {
				LOGGER.info("POST-CONDITION 2 : ACTUAL : SUCCESSFULLY SET THE DEFAULT OPERATING STANDARD");
			} else {
				LOGGER.info("POST-CONDITION 2 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-RADIO-3002");
	}

	/**
	 * Test method to set the operating standard in LAN GUI ADMIN page
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param wifiBand              Frequency band 2.4/5 GHz
	 * @param stepNumber            Test step number
	 * @param wifiOperatingStandard Operating standard for 2.4/5 GHz
	 * @param driver                WebDriver
	 * @param lanSidePageNavigation Instance of{@link LanSidePageNavigation}
	 * @param operStd               Operating standard value for 2.4/5 GHz
	 * @param broadBandCommonPage   Instance of{@link BroadBandCommonPage}
	 * 
	 * @Refactor Sruthi Santhosh
	 *
	 */
	public static void executeTestStepToSetOperatingStandard(Dut device, String testCaseId, String wifiBand,
			int stepNumber, String wifiOperatingStandard, WebDriver driver, LanSidePageNavigation lanSidePageNavigation,
			String operStd, BroadBandCommonPage broadBandCommonPage) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;

		/**
		 * Step : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI > EDIT 2.4/5 GHZ PAGE AND
		 * VERIFY NAVIGATION STATUS
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI > EDIT "
				+ wifiBand + "GHZ PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > WI-FI > EDIT " + wifiBand
				+ "GHZ PAGE");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > WI-FI > EDIT "
				+ wifiBand + "GHZ PAGE");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > WI-FI > EDIT" + wifiBand
				+ "GHZ PAGE";
		status = lanSidePageNavigation.navigateToPrivateWiFiEditPage(device, tapEnv, driver, wifiBand);
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > WI-FI > EDIT "
							+ wifiBand + "GHZ PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		/**
		 * Step : SET THE OPERATION STANDARD MODE IN LAN GUI ADMIN PAGE.
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : SET THE OPERATION STANDARD MODE AS "
				+ wifiOperatingStandard + " IN LAN GUI ADMIN PAGE.");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > WI-FI > EDIT " + wifiBand
				+ "GHZ PAGE AND SET THE MODE AS " + wifiOperatingStandard);
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > WI-FI > EDIT "
				+ wifiBand + "GHZ PAGE AND ABLE TO CHANGE THE MODE TO " + wifiOperatingStandard);
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO CHANGE THE OPERATION STANDARD MODE AS " + wifiOperatingStandard
				+ " IN LAN GUI ADMIN PAGE";
		try {
			status = broadBandCommonPage.selectElementFromDropDownByVisibleText(
					BroadBandWebGuiElements.ELEMEMT_ID_2GHZ_MODE_DROP_DOWN, operStd, operStd,
					BroadBandWebGuiElements.XPATH_WIFI_MODE);
			broadBandCommonPage.click(By.xpath(BroadBandWebGuiTestConstant.XPATH_SAVE_2_4_GHZ_NETWORK_PASSWORD));
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error("Exception occurred during changing the operation mode  : " + errorMessage);
		}

		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS "
					+ wifiOperatingStandard + " IN LAN GUI ADMIN PAGE");
			LOGGER.info("Going to wait for 1 minute for the WiFi Changes to reflect!");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
	}

	/**
	 * Test method used to validate the operating standard for 2.4 and 5 GHz
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param wifiBand              Frequency band 2.4/5 GHz
	 * @param stepNumber            Test step number
	 * @param wifiOperatingStandard Operating standard for 2.4/5 GHz
	 * @param driver                Instance of{@link WebDriver}
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static void executeTestStepToVerifyOperatingStandard(Dut device, String testCaseId, String wifiBand,
			int stepNumber, String wifiOperatingStandard, WebDriver driver) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String webpaResponse = null;

		wifiOperatingStandard = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadbandPropertyFileHandler.get2GhzOperatingModeForRPi()
				: BroadbandPropertyFileHandler.get5GhzOperatingModeForRPi();

		String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD
				: BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD;
		/**
		 * Step : VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF 2.4/5 GHZ USING
		 * WEBPA.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF "
				+ wifiBand + " WITH OPERATIONAL STANDARD AS " + wifiOperatingStandard + " USING WEBPA");
		LOGGER.info("STEP " + stepNumber + " : ACTION : VERIFY THE VALUE OF OPERATING STANDARD FOR USING WEBPA PARAM "
				+ operStdWepParam);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : IT SHOULD RETURN THE VALUE OF OPERATING STANDARD AS "
				+ wifiOperatingStandard);
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY OPERATING STANDARD AS " + wifiOperatingStandard;
		webpaResponse = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, operStdWepParam);
		status = CommonMethods.isNotNull(webpaResponse) && webpaResponse.equalsIgnoreCase(wifiOperatingStandard);
		if (!status) {
			LOGGER.info(
					"UNABLE TO SET THE OPERATING STANDARD FROM WEBUI ADMIN PAGE,TRYING TO SET THE OPERATING STANDARD USING WEBPA");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv, operStdWepParam,
					BroadBandTestConstants.CONSTANT_0, wifiOperatingStandard,
					BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE OPERATING STANDARD AS "
					+ wifiOperatingStandard);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

	}

	/**
	 * Test method used to validate the basic data transmission rates for 2.4 and 5
	 * GHz
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param wifiBand              Frequency band 2.4/5 GHz
	 * @param stepNumber            Test step number
	 * @param wifiOperatingStandard Operating standard for 2.4/5 GHz
	 * @param driver                Instance of{@link WebDriver}
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static String executeTestStepToVerifyBasicDataTxRates(Dut device, String testCaseId, String wifiBand,
			int stepNumber, String wifiOperatingStandard, WebDriver driver) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String basicDataTransmission = null;
		String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES
				: BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES;
		/**
		 * Step : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF 2.4/5 GHZ USING
		 * WEBPA.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF "
				+ wifiBand + " WITH OPERATIONAL STANDARD AS " + wifiOperatingStandard + " USING WEBPA");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES FOR USING WEBPA PARAM "
				+ operStdWepParam);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : IT SHOULD RETURN THE VALUE OF BASIC DATA TRANSMISSION RATES");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY BASIC DATA TRANSMISSION RATES";
		basicDataTransmission = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				operStdWepParam);
		status = CommonMethods.isNotNull(basicDataTransmission);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE BASIC DATA TRANSMISSION RATES");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		return basicDataTransmission;
	}

	/**
	 * Test method used to validate the supported data transmission rates for 2.4
	 * and 5 GHz
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param wifiBand              Frequency band 2.4/5 GHz
	 * @param stepNumber            Test step number
	 * @param wifiOperatingStandard Operating standard for 2.4/5 GHz
	 * @param driver                Instance of{@link WebDriver}
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static String executeTestStepToVerifySupportedDataTxRates(Dut device, String testCaseId, String wifiBand,
			int stepNumber, String wifiOperatingStandard, WebDriver driver) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String supportedDataTransmission = null;
		String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_4_SUPPORTED_DATA_TX_RATES
				: BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES;
		/**
		 * Step : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 2.4/5 GHZ
		 * USING WEBPA.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF "
				+ wifiBand + " WITH OPERATIONAL STANDARD AS " + wifiOperatingStandard + " USING WEBPA");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES FOR USING WEBPA PARAM "
				+ operStdWepParam);
		LOGGER.info(
				"STEP " + stepNumber + " : EXPECTED : IT SHOULD RETURN THE VALUE OF SUPPORTED DATA TRANSMISSION RATES");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY SUPPORTED DATA TRANSMISSION RATES";
		supportedDataTransmission = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				operStdWepParam);
		status = CommonMethods.isNotNull(supportedDataTransmission);
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE SUPPORTED DATA TRANSMISSION RATES");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		return supportedDataTransmission;
	}

	/**
	 * Test method used to validate the operational data transmission rates for 2.4
	 * and 5 GHz
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param wifiBand              Frequency band 2.4/5 GHz
	 * @param stepNumber            Test step number
	 * @param wifiOperatingStandard Operating standard for 2.4/5 GHz
	 * @param driver                Instance of{@link WebDriver}
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static String executeTestStepToVerifyOperationalDataTxRates(Dut device, String testCaseId, String wifiBand,
			int stepNumber, String wifiOperatingStandard, WebDriver driver) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String oparationalDataTransmission = null;
		String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES
				: BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES;
		/**
		 * Step : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 2.4/5 GHZ
		 * USING WEBPA.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP " + stepNumber + " : DESCRIPTION : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES OF "
						+ wifiBand + " WITH OPERATIONAL STANDARD AS " + wifiOperatingStandard + " USING WEBPA");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES FOR USING WEBPA PARAM "
				+ operStdWepParam);
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : IT SHOULD RETURN THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY OPERATIONAL DATA TRANSMISSION RATES";
		oparationalDataTransmission = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				operStdWepParam);
		status = CommonMethods.isNotNull(oparationalDataTransmission);
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE OPERATIONAL DATA TRANSMISSION RATES");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		return oparationalDataTransmission;
	}

	/**
	 * Test method to reset the device
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param stepNumber            Test step number
	 * @param driver                WebDriver
	 * @param lanSidePageNavigation Instance of{@link LanSidePageNavigation}
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static void executeTestStepToResetDevice(Dut device, String testCaseId, int stepNumber, WebDriver driver,
			LanSidePageNavigation lanSidePageNavigation) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		/**
		 * Step : NAVIGATE TO THE TROUBLESHOOTING > RESET/RESTORE GATEWAY PAGE AND
		 * VERIFY NAVIGATION STATUS.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE TROUBLESHOOTING > RESET/RESTORE GATEWAY PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON TROUBLESHOOTING > RESET/RESTORE GATEWAY PAGE");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE TROUBLESHOOTING > RESET/RESTORE GATEWAY PAGE");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON TROUBLESHOOTING > RESET/RESTORE GATEWAY PAGE";
		status = lanSidePageNavigation.navigateToTroubleShootingResetRestorePage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR TROUBLESHOOTING > RESET/RESTORE GATEWAY PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		/**
		 * Step : RESET THE DEVICE THROUGH LAN GUI ADMIN PAGE.
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		BroadBandResetRestoreGatewayPage broadBandResetRestoreGatewayPage = new BroadBandResetRestoreGatewayPage(
				driver);
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : RESET THE DEVICE THROUGH LAN GUI ADMIN PAGE.");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION :  CLICK ON TROUBLESHOOTING > RESET/RESTORE GATEWAY PAGE AND CLICK ON RESET");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : DEVICE SHOULD REBOOT");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO RESTORE WIFI SETTINGS.";
		broadBandResetRestoreGatewayPage.resetGatewaySettingsBasedOnRequest(RestoreGateWay.RESTART_GATEWAY);
		errorMessage = "DEVICE IS NOT ACCESSIBLE AFTER RESET,EVEN AFTER FIVE MINUTES";
		status = BroadBandCommonUtils.waitAndCheckDeviceResetInDuration(device, tapEnv,
				BroadBandTestConstants.TEN_MINUTE_IN_MILLIS);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : PERFORMED RESET SUCCESSFULLY.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);
	}

	/**
	 * Test method used to validate the basic data transmission rates for 2.4 and 5
	 * GHz after reboot
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param wifiBand              Frequency band 2.4/5 GHz
	 * @param stepNumber            Test step number
	 * @param wifiOperatingStandard Operating standard for 2.4/5 GHz
	 * @param driver                Instance of{@link WebDriver}
	 * @param basicDataTransmission Basic Data Transmission Rates before Reboot
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static void executeTestStepToVerifyBasicDataTxRatesAfterReboot(Dut device, String testCaseId,
			String wifiBand, int stepNumber, String wifiOperatingStandard, WebDriver driver,
			String basicDataTransmission) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String webpaResponse = null;
		String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES
				: BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES;
		/**
		 * Step : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF 2.4/5 GHZ USING
		 * WEBPA.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF "
				+ wifiBand + " WITH OPERATIONAL STANDARD AS " + wifiOperatingStandard + " AFTER REBOOT USING WEBPA");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES FOR USING WEBPA PARAM "
				+ operStdWepParam);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : IT SHOULD RETURN THE VALUE OF BASIC DATA TRANSMISSION RATES");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY BASIC DATA TRANSMISSION RATES AFTER REBOOT";
		webpaResponse = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, operStdWepParam);
		status = CommonMethods.isNotNull(webpaResponse)
				&& BroadBandCommonUtils.compareCommaSeparateValues(basicDataTransmission, webpaResponse);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : SUCCESSFULLY VERIFIED THE BASIC DATA TRANSMISSION RATES AFTER REBOOT");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
	}

	/**
	 * Test method used to validate the supported data transmission rates for 2.4
	 * and 5 GHz after reboot
	 * 
	 * @param device                    Instance of{@link Dut}
	 * @param testCaseId                Test case ID
	 * @param wifiBand                  Frequency band 2.4/5 GHz
	 * @param stepNumber                Test step number
	 * @param wifiOperatingStandard     Operating standard for 2.4/5 GHz
	 * @param driver                    Instance of{@link WebDriver}
	 * @param supportedDataTransmission Supported Data Transmission Rates before
	 *                                  Reboot
	 * @Refactor Sruthi Santhosh
	 */
	public static void executeTestStepToVerifySupportedDataTxRatesAfterReboot(Dut device, String testCaseId,
			String wifiBand, int stepNumber, String wifiOperatingStandard, WebDriver driver,
			String supportedDataTransmission) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String webpaResponse = null;
		String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_4_SUPPORTED_DATA_TX_RATES
				: BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES;
		/**
		 * Step : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 2.4/5 GHZ
		 * AFTER REBOOT USING WEBPA.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF "
				+ wifiBand + " WITH OPERATIONAL STANDARD AS " + wifiOperatingStandard + " AFTER REBOOT USING WEBPA");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES FOR USING WEBPA PARAM "
				+ operStdWepParam);
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : IT SHOULD RETURN THE VALUE OF SUPPORTED DATA TRANSMISSION RATES AFTER REBOOT");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY SUPPORTED DATA TRANSMISSION RATES AFTER REBOOT";
		webpaResponse = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, operStdWepParam);
		status = CommonMethods.isNotNull(webpaResponse)
				&& BroadBandCommonUtils.compareCommaSeparateValues(supportedDataTransmission, webpaResponse);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : SUCCESSFULLY VERIFIED THE SUPPORTED DATA TRANSMISSION RATES AFTER REBOOT");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
	}

	/**
	 * Test method used to validate the operational data transmission rates for 2.4
	 * and 5 GHz after reboot
	 * 
	 * @param device                      Instance of{@link Dut}
	 * @param testCaseId                  Test case ID
	 * @param wifiBand                    Frequency band 2.4/5 GHz
	 * @param stepNumber                  Test step number
	 * @param wifiOperatingStandard       Operating standard for 2.4/5 GHz
	 * @param driver                      Instance of{@link WebDriver}
	 * @param oparationalDataTransmission Operational Data Transmission Rates before
	 *                                    Reboot
	 * @Refactor Sruthi Santhosh
	 */
	public static void executeTestStepToVerifyOperationalDataTxRatesAfterReboot(Dut device, String testCaseId,
			String wifiBand, int stepNumber, String wifiOperatingStandard, WebDriver driver,
			String oparationalDataTransmission) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String webpaResponse = null;
		String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES
				: BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES;
		/**
		 * Step : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 2.4/5 GHZ
		 * AFTER REBOOT USING WEBPA.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES OF " + wifiBand
				+ " WITH OPERATIONAL STANDARD AS " + wifiOperatingStandard + " AFTER REBOOT USING WEBPA");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES FOR USING WEBPA PARAM "
				+ operStdWepParam);
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : IT SHOULD RETURN THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES AFTER REBOOT");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY OPERATIONAL DATA TRANSMISSION RATES AFTER REBOOT";
		webpaResponse = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, operStdWepParam);
		status = CommonMethods.isNotNull(webpaResponse)
				&& BroadBandCommonUtils.compareCommaSeparateValues(oparationalDataTransmission, webpaResponse);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : SUCCESSFULLY VERIFIED THE OPERATIONAL DATA TRANSMISSION RATES AFTER REBOOT");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
	}

	/**
	 *
	 * Test Case : Verify the configuration of Transmission Control Rate for
	 * 5Ghz(a,n,ac) Radio after reboot.
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 2.4/5 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>PRE-CONDITION 6 : Reactive the router device</li>
	 * <li>PRE-CONDITION 7 : Get the default operating standards</li>
	 * <li>Step 1 : Verify login into the LAN GUI Adimn page by using valid userid
	 * and valid password</li>
	 * <li>Step 2 : Navigate to the Gateway>Connection>WIFI and verify Navigation
	 * Status</li>
	 * <li>Step 3 : Navigate to the Gateway>Connection>WIFI >Edit 5 Ghz and verify
	 * Navigation Status</li>
	 * <li>Step 4 : Set the operation standard mode as a/n/ac in LAN GUI Adimn
	 * page</li>
	 * <li>Step 5 : Verify the value of operational transmission rate of the 5 GHz
	 * with operating standard as a/n/ac using webpa</li>
	 * <li>Step 6 : Verify the value of Basic data transmission rates of the 5GHz
	 * with operating standard as a/n/ac using webpa</li>
	 * <li>Step 7 : Verify the value of Supported data transmission rates of the
	 * 5GHz with operating standard as a/n/ac using webpa</li>
	 * <li>Step 8 : Verify the value of Operational data transmission Rates of the
	 * 5GHz with operating standard as a/n/ac using webpa</li>
	 * <li>Step 9 : Navigate to the Troubleshooting>Reset/Restore Gateway page and
	 * verify Navigation Status</li>
	 * <li>Step 10 : Reset the device through LAN GUI Adimn page.</li>
	 * <li>Step 11 : Verify the value of operational transmission rate of the 5GHz
	 * with operating standard as a/n/ac after reboot using webpa</li>
	 * <li>Step 12 : Verify the value of Basic data transmission rates of the 5GHz
	 * with operating standard as a/n/ac after reboot with the value of step 6</li>
	 * <li>Step 13 : Verify the value of Supported data transmission rates of the
	 * 5GHz with operating standard as a/n/ac after reboot with the value of step
	 * 7</li>
	 * <li>Step 14 : Verify the value of Operational data transmission Rates of the
	 * 5GHz with operating standard as a/n/ac after * reboot with the value of step
	 * 8</li>
	 * <li>POST-CONDITION 1 : Close the browser</li>
	 * <li>POST-CONDITION 2 : Set the default operating standards</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Rajapandian
	 * 
	 * @Refactor Sruthi Santhosh
	 *
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-RADIO-3001")
	public void testToVerifyTxRateConfigInWebUiForWifi1(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-RADIO-301";
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		String errorMessage = "";
		boolean status = false;
		WebDriver driver = null;
		LanSidePageNavigation lanSidePageNavigation = null;
		BroadBandCommonPage broadBandCommonPage = null;
		Dut deviceConnected = null;
		String basicDataTransmission5ghz = null;
		String supportedDataTransmission5ghz = null;
		String oparationalDataTransmission5ghz = null;
		String defaultOperatingStandard5ghz = null;
		// Variable Declation Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-RADIO-3001");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the configuration of Transmission Control Rate for 5Ghz(a,n,ac) Radio after reboot.");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 2.4/5 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify  the correct IPv4  address for client connected with 2.4/5 GHz SSID");
		LOGGER.info("PRE-CONDITION 3 : Verify  the correct IPv6  address for client connected with 2.4/5 GHz SSID");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
		LOGGER.info("PRE-CONDITION 6 : Reactive the router device");
		LOGGER.info("PRE-CONDITION 7 : Get the default operating standards");
		LOGGER.info("Step 1. Verify login into the LAN GUI Adimn page by using valid userid and valid password");
		LOGGER.info("Step 2. Navigate to the Gateway>Connection>WIFI and verify Navigation Status");
		LOGGER.info("Step 3. Navigate to the Gateway>Connection>WIFI >Edit 5 Ghz and verify Navigation Status");
		LOGGER.info("Step 4. Set the operation standard mode as a/n/ac in  LAN GUI Adimn page");
		LOGGER.info(
				"Step 5. Verify  the value of operational transmission rate of the 5 GHz with operating standard as a/n/ac  using webpa");
		LOGGER.info(
				"Step 6. Verify  the value of Basic data transmission rates of the 5GHz with operating standard as a/n/ac using webpa");
		LOGGER.info(
				"Step 7. Verify  the value of Supported data transmission rates of the 5GHz with operating standard as a/n/ac using webpa");
		LOGGER.info(
				"Step 8. Verify  the value of Operational data transmission Rates of the 5GHz with operating standard as a/n/ac using webpa");
		LOGGER.info("Step 9. Navigate to the Troubleshooting>Reset/Restore Gateway page and verify Navigation Status");
		LOGGER.info("Step 10. Reset the device through  LAN GUI Adimn page.");
		LOGGER.info(
				"Step 11. Verify the value of operational transmission rate of the 5GHz with operating standard as a/n/ac after reboot using webpa.");
		LOGGER.info(
				"Step 12. Verify the value of Basic data transmission rates of the 5GHz with operating standard as a/n/ac after reboot with the value of step 6");
		LOGGER.info(
				"Step 13. Verify the value of Supported data transmission rates of the 5GHz with operating standard as a/n/ac after reboot with the value of step 7");
		LOGGER.info(
				"Step 14. Verify the value of Operational data transmission Rates of the 5GHz with operating standard as a/n/ac after reboot with the value of step 8");
		LOGGER.info("POST-CONDITION 1 : Close the browser");
		LOGGER.info("POST-CONDITION 2 : Set the default operating standards");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRECONDITION 1-6 :
			 */
			deviceConnected = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
					BroadBandTestConstants.BAND_2_4GHZ_OR_5GHZ);
			BroadBandPreConditionUtils.executePreConditionToReacitivateDevice(device, tapEnv,
					BroadBandTestConstants.CONSTANT_6);
			/**
			 * PRECONDITION 7 : GET THE DEFAULT OPERATING STANDARDS.
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 7 : DESCRIPTION : GET THE DEFAULT OPERATING STANDARDS");
			LOGGER.info("PRE-CONDITION 7 : ACTION : GET THE DEFAULT OPERATING STANDARDS USING WEBPA");
			LOGGER.info("PRE-CONDITION 7 : EXPECTED : MUST RETRIEVE THE DEFAULT OPERATING STANDARDS VALUES");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET DEFAULT OPERATING STANDARDS";
			defaultOperatingStandard5ghz = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
			if (CommonMethods.isNotNull(defaultOperatingStandard5ghz)) {
				LOGGER.info("PRE-CONDITION 7 : ACTUAL: DEFAULT OPERATING STANDARDS ARE RETRIEVED SUCCESSFULLY.");
			} else {
				LOGGER.error("PRE-CONDITION 7 : ACTUAL: " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : 7 : FAILED : " + errorMessage);
			}

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING VALID USERID AND
			 * VALID PASSWORD
			 */
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : VERIFY THE GATEWAY ADMIN PAGE IS ACCESSIBLE IN CONNECTED CLIENT AND CAN BE LOGGED IN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : LAUNCH THE GATEWAY ADMIN GUI IN BROWSER URL : https://10.0.0.1 or https://10.1.10.1 , ONCE THE PAGE IS LOADED ,USE USERNAME AND PASSWORD AS ADMIN/****** FOR RESIDENTIAL OR CUSADMIN/****** FOR COMMERCIAL DEVICES TO LOGIN");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : GATEWAY ADMIN PAGE SHOULD BE ACCESSIBLE FROM CLIENT AND CAN BE ABLE TO LOGIN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO LOGIN GATEWAY ADMIN PAGE IN CONNECTED CLIENT";
			try {
				status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
				isBrowserOpen = status;
				driver = LanWebGuiLoginPage.getDriver();
				LOGGER.info(" webDriver " + driver);
				lanSidePageNavigation = new LanSidePageNavigation(driver);
				broadBandCommonPage = new BroadBandCommonPage(driver);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error("Exception occurred during Gateway Admin Page login : " + errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : LAN GUI ADMIN LOGIN SUCCESSFUL");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 2 : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI PAGE AND VERIFY
			 * NAVIGATION STATUS
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI PAGE AND VERIFY NAVIGATION STATUS");
			LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > WI-FI");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > WI-FI PAGE");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > WI-FI PAGE";
			LanSideWiFiPage lanSideWiFiPage = new LanSideWiFiPage(driver);
			try {
				if (lanSideWiFiPage.navigateToConnection(device, tapEnv)) {
					status = lanSideWiFiPage.navigateToWiFiPage(device, tapEnv);
				}
			} catch (Exception exception) {
				LOGGER.error("Exception in launching WiFi page " + exception.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > WI-FI PAGE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 3-4 : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI > EDIT 5 GHZ PAGE AND
			 * SET THE OPERATION STANDARD MODE AS a/n/ac IN LAN GUI ADMIN PAGE.
			 */
			stepNumber++;
			executeTestStepToSetOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC, driver, lanSidePageNavigation,
					BroadBandTestConstants.WIRELESS_MODE_A_N_AC, broadBandCommonPage);

			/**
			 * Step 5 : VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS a/n/ac USING WEBPA.
			 */
			stepNumber = 5;
			executeTestStepToVerifyOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC, driver);

			/**
			 * Step 6 : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS a/n/ac USING WEBPA.
			 */
			stepNumber++;
			basicDataTransmission5ghz = executeTestStepToVerifyBasicDataTxRates(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC,
					driver);

			/**
			 * Step 7 : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS a/n/ac USING WEBPA.
			 */
			stepNumber++;
			supportedDataTransmission5ghz = executeTestStepToVerifySupportedDataTxRates(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC,
					driver);

			/**
			 * Step 8 : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES OF 5 GHZ
			 * WITH OPERATIONAL STANDARD AS a/n/ac USING WEBPA.
			 */
			stepNumber++;
			oparationalDataTransmission5ghz = executeTestStepToVerifyOperationalDataTxRates(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC,
					driver);

			/**
			 * Step 9-10 : RESET THE DEVICE THROUGH LAN GUI ADMIN PAGE.
			 */
			stepNumber++;
			executeTestStepToResetDevice(device, testCaseId, stepNumber, driver, lanSidePageNavigation);

			/**
			 * Step 11 : VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS a/n/ac AFTER REBOOT USING WEBPA.
			 */
			stepNumber = 11;
			executeTestStepToVerifyOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC, driver);

			/**
			 * Step 12 : VERIFY THE VALUE OF BASIC DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS a/n/ac AFTER REBOOT WITH THE VALUE OF STEP 6.
			 */
			stepNumber++;
			executeTestStepToVerifyBasicDataTxRatesAfterReboot(device, testCaseId, BroadBandTestConstants.BAND_5GHZ,
					stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC, driver, basicDataTransmission5ghz);

			/**
			 * Step 13 : VERIFY THE VALUE OF SUPPORTED DATA TRANSMISSION RATES OF 5 GHZ WITH
			 * OPERATIONAL STANDARD AS a/n/ac AFTER REBOOT WITH THE VALUE OF STEP 7.
			 */
			stepNumber++;
			executeTestStepToVerifySupportedDataTxRatesAfterReboot(device, testCaseId, BroadBandTestConstants.BAND_5GHZ,
					stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC, driver,
					supportedDataTransmission5ghz);

			/**
			 * Step 14 : VERIFY THE VALUE OF OPERATIONAL DATA TRANSMISSION RATES OF 5 GHZ
			 * WITH OPERATIONAL STANDARD AS a/n/ac AFTER REBOOT WITH THE VALUE OF STEP 8.
			 */
			stepNumber++;
			executeTestStepToVerifyOperationalDataTxRatesAfterReboot(device, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, stepNumber, BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC,
					driver, oparationalDataTransmission5ghz);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.info(
					"EXCEPTION OCCURRED WHILE VALIDATING testToVerifyTxRateConfigInWebUiForWifi():" + e.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST CONDITION 1 : CLOSE THE LAN SIDE BROWSER.
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("#######################################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			/**
			 * POST CONDITION 2 : SET THE DEFAULT OPERATING STANDARD.
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 2 : DESCRIPTION : SET THE DEFAULT OPERATING STANDARD");
			LOGGER.info("POST-CONDITION 2 : ACTION : SET THE DEFAULT OPERATING STANDARD USING WEBPA");
			LOGGER.info("POST-CONDITION 2 : EXPECTED : MUST SET THE DEFAULT OPERATING STANDARD ");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO SET DEFAULT OPERATING STANDARD";
			boolean isdefaultOperStd5ghz = false;
			if (CommonMethods.isNotNull(defaultOperatingStandard5ghz)) {
				isdefaultOperStd5ghz = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						BroadBandTestConstants.CONSTANT_0, defaultOperatingStandard5ghz,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			}
			if (isdefaultOperStd5ghz) {
				LOGGER.info("POST-CONDITION 2 : ACTUAL : SUCCESSFULLY SET THE DEFAULT OPERATING STANDARD");
			} else {
				LOGGER.info("POST-CONDITION 2 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-RADIO-3001");
	}

	/**
	 * Post-Condition method to set the dhcp ipv4 values using webpa .
	 * 
	 * <li></li>
	 * <li>Set the default Ipv4 SubnetMask Address</li>
	 * <li>Set the default Ipv4 BeginAddress Address</li>
	 * <li>Set the default Ipv4 EndingAddress Address</li>
	 * <li>Set the default Ipv4 DhcpLeaseTime Address</li>
	 * 
	 * @param device                   {@link Dut}
	 * @param defaultDchpIpv4ValuesMap Its used to get the key and pair value for
	 *                                 default dhcp v4 values
	 * @return postCondStatus True ,IF all default values are set successfully. Else
	 *         False
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static boolean executePostConditionToSetTheDefaultDchpIpv4Values(Dut device,
			HashMap<String, String> defaultDchpIpv4ValuesMap) {
		boolean postCondStatus = false;
		try {
			if (defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_IPV4_SUBNET_MASK) != null) {
				postCondStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_LAN_IPv4_SUBNET_MASK, BroadBandTestConstants.CONSTANT_0,
						defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_IPV4_SUBNET_MASK));
			}
			if (defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_BEGIN_ADDRESS) != null) {
				postCondStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_MINADDRESS, BroadBandTestConstants.CONSTANT_0,
						defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_BEGIN_ADDRESS));
			}
			if (defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_ENDING_ADDRESS) != null) {
				postCondStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_MAXADDRESS, BroadBandTestConstants.CONSTANT_0,
						defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_ENDING_ADDRESS));
			}
			if (defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPLEASE_TIME) != null) {
				postCondStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
						defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPLEASE_TIME));
			}
		} catch (Exception exec) {
			LOGGER.error("Failed to Set the default DHCP IPv4 Values in Post Condition " + exec.getLocalizedMessage());
		}
		return postCondStatus;
	}

	/**
	 *
	 * Test Case : Verify Admin GUI login and verify all pages are accessible in
	 * Firefox Browser for 2.4 GHz wifi
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 2.4 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 2.4 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>Step 1: Verify login into the LAN GUI Admin page by using valid userid
	 * and valid password</li>
	 * <li>Step 2: Navigate to the Gateway > At a Glance page and verify navigation
	 * status</li>
	 * <li>Step 3: Navigate to the Gateway > Connection > Status page and verify
	 * navigation status</li>
	 * <li>Step 4: Navigate to the Gateway> Connection> PARTNER Network page
	 * (Residential) OR Gateway> Connection> Partner Network page(Commercial) and
	 * verify Navigation Status</li>
	 * <li>Step 5: Navigate to the Gateway > Connection > Local IP Configuration
	 * page and verify navigation status</li>
	 * <li>Step 6: Navigate to the Gateway > Connection > Wi-Fi page and verify
	 * navigation status</li>
	 * <li>Step 7: Navigate to the Gateway > Connection > MoCA page and verify
	 * navigation status</li>
	 * <li>Step 8 : Navigate to the Gateway > Firewall > IPv4 page and verify
	 * navigation status</li>
	 * <li>Step 9 : Navigate to the Gateway > Firewall > IPv6 page and verify
	 * navigation status</li>
	 * <li>Step 10 : Navigate to the Gateway > Software page and verify navigation
	 * status</li>
	 * <li>Step 11 : Navigate to the Gateway > Hardware > System Hardware page</li>
	 * <li>Step 12 : Navigate to the Gateway > Hardware > LAN Ethernet page and
	 * verify navigation status</li>
	 * <li>Step 13 : Navigate to the Gateway > Hardware > Wireless page and verify
	 * navigation status</li>
	 * <li>Step 14 : Navigate to the Connected Devices > Devices page and verify
	 * navigation status</li>
	 * <li>Step 15 : Navigate to the Parental Control > Managed Sites page and
	 * verify navigation status</li>
	 * <li>Step 16 : Navigate to the Parental Control > Managed Services page and
	 * verify navigation status</li>
	 * <li>Step 17 : Navigate to the Parental Control > Managed Devices page and
	 * verify navigation status</li>
	 * <li>Step 18 : Navigate to the Parental Control > Reports page and verify
	 * navigation status</li>
	 * <li>Step 19 : Navigate to the Advanced > Port Forwarding page and verify
	 * navigation status</li>
	 * <li>Step 20 : Navigate to the Advanced > Port Triggering page and verify
	 * navigation status</li>
	 * <li>Step 21 : Navigate to the Advanced > Remote Management page and verify
	 * navigation status</li>
	 * <li>Step 22 : Navigate to the Advanced > DMZ page and verify navigation
	 * status</li>
	 * <li>Step 23 : Navigate to the Advanced > Device Discovery page and verify
	 * navigation status</li>
	 * <li>Step 24 : Navigate to the Troubleshooting > Logs page and verify
	 * navigation status</li>
	 * <li>Step 25 : Navigate to the Troubleshooting > Network Diagnostic Tools page
	 * and verify navigation status</li>
	 * <li>Step 26 : Navigate to the Troubleshooting > Wi-Fi Spectrum Analyzer page
	 * and verify navigation status</li>
	 * <li>Step 27 : Navigate to the Troubleshooting > MOCA Diagnostics page and
	 * verify navigation status</li>
	 * <li>Step 28 : Navigate to the Troubleshooting > Reset/Restore Gateway page
	 * and verify navigation status</li>
	 * <li>Step 29 : Navigate to the Troubleshooting > Change Password page and
	 * verify navigation status</li>
	 * <li>POST-CONDITION 1 : Close the Browser LAN Client</li>
	 * <li>POST-CONDITION 2 : Verify disconnecting the 2.4 GHz private wifi
	 * SSID</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Vignesh
	 * @refactor Said Hisham
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WEB-GUI-ADMIN-6001")
	public void testToVerifyAllLanSidePageLaunchStatusInFirefoxBrowserUsing2GHz(Dut device) {
		Dut deviceConnectedWith2Ghz = null;
		String testCaseId = "TC-RDKB-WEB-GUI-ADMIN-601";
		int stepNumber = 1;
		stepNum = "S" + stepNumber;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WEB-GUI-ADMIN-6001");
		LOGGER.info(
				"TEST DESCRIPTION : Verify Admin GUI login and verify all pages are accessible in Firefox Browser for 2.4 GHz wifi");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify IPv4 is assigned on the 2.4GHz Wi-Fi client");
		LOGGER.info("PRE-CONDITION 3 : Verify IPv6 is assigned on the 2.4GHz Wi-Fi client");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected 2.4GHz Wi-Fi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected 2.4GHz Wi-Fi client using ipv6 interface");
		LOGGER.info(" 1: Verify login into the LAN GUI Adimn page by using valid userid and valid password");
		LOGGER.info(" 2: Navigate to the Gateway > At a Glance page and verify navigation status");
		LOGGER.info(" 3: Navigate to the Gateway > Connection > Status page and verify navigation status");
		LOGGER.info(
				" 4: Navigate to the Gateway>Connection>PARTNER Network page (Residential)  OR Gateway>Connection>Partner Network page(Commercial) and verify Navigation Status");
		LOGGER.info(
				" 5: Navigate to the Gateway > Connection > Local IP Configuration page and verify navigation status");
		LOGGER.info(" 6: Navigate to the Gateway > Connection > Wi-Fi page and verify navigation status");
		LOGGER.info(" 7: Navigate to the Gateway > Connection > MoCA page and verify navigation status");
		LOGGER.info(" 8 : Navigate to the Gateway > Firewall > IPv4 page and verify navigation status");
		LOGGER.info(" 9 : Navigate to the Gateway > Firewall > IPv6 page and verify navigation status");
		LOGGER.info(" 10 : Navigate to the Gateway > Software  page and verify navigation status");
		LOGGER.info(" 11 : Navigate to the Gateway > Hardware > System Hardware page");
		LOGGER.info(" 12 : Navigate to the Gateway > Hardware > LAN Ethernet page and verify navigation status");
		LOGGER.info(" 13 : Navigate to the Gateway > Hardware > Wireless page and verify navigation status");
		LOGGER.info(" 14 : Navigate to the Connected Devices > Devices page and verify navigation status");
		LOGGER.info(" 15 : Navigate to the Parental Control > Managed Sites page and verify navigation status");
		LOGGER.info(" 16 : Navigate to the Parental Control > Managed Services page and verify navigation status");
		LOGGER.info(" 17 : Navigate to the Parental Control > Managed Devices page and verify navigation status");
		LOGGER.info(" 18 : Navigate to the Parental Control > Reports page and verify navigation status");
		LOGGER.info(" 19 : Navigate to the Advanced > Port Forwarding page and verify navigation status");
		LOGGER.info(" 20 : Navigate to the Advanced > Port Triggering page and verify navigation status");
		LOGGER.info(" 21 : Navigate to the Advanced > Remote Management page and verify navigation status");
		LOGGER.info(" 22 : Navigate to the Advanced > DMZ page and verify navigation status");
		LOGGER.info(" 23 : Navigate to the Advanced > Device Discovery  page and verify navigation status");
		LOGGER.info(" 24 : Navigate to the Troubleshooting > Logs  page and verify navigation status");
		LOGGER.info(
				" 25 : Navigate to the Troubleshooting > Network Diagnostic Tools page and verify navigation status");
		LOGGER.info(
				" 26 : Navigate to the Troubleshooting > Wi-Fi Spectrum Analyzer page and  verify navigation status");
		LOGGER.info(" 27 : Navigate to the Troubleshooting >   Diagnostics page and  verify navigation status");
		LOGGER.info(" 28 : Navigate to the Troubleshooting > Reset/Restore Gateway  page and verify navigation status");
		LOGGER.info(" 29 : Navigate to the Troubleshooting > Change Password page and verify navigation status");
		LOGGER.info("POST-CONDITION 1 : Close the Browser LAN Client");
		LOGGER.info("POST-CONDITION 2 : Verify disconnecting the 2.4 GHz private wifi SSID");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnectedWith2Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
					tapEnv, BroadBandTestConstants.BAND_2_4GHZ);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			validateLanSidePageNavigationCrossBroserValidation(device, deviceConnectedWith2Ghz, testCaseId,
					Browser.FIREFOX);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-condition 1 : Close the LAN Side Browser
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("#######################################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			/**
			 * Post-condition 2 : Disconnect the 2.4 GHz private wifi SSID
			 */
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith2Ghz, BroadBandTestConstants.CONSTANT_2);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEB-GUI-ADMIN-6001");
	}

	/**
	 *
	 * Test Case : Verify Admin GUI login and verify all pages are accessible in
	 * Firefox Browser for 5GHz wifi
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 5 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 5GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 5 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected 5GHz
	 * Wi-Fi client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected 5GHz
	 * Wi-Fi client using ipv6 interface</li>
	 * <li>Step 1: Verify login into the LAN GUI Admin page by using valid userid
	 * and valid password</li>
	 * <li>Step 2: Navigate to the Gateway > At a Glance page and verify navigation
	 * status</li>
	 * <li>Step 3: Navigate to the Gateway > Connection > Status page and verify
	 * navigation status</li>
	 * <li>Step 4: Navigate to the Gateway> Connection> PARTNER Network page
	 * (Residential) OR Gateway> Connection> Partner Network page(Commercial) and
	 * verify Navigation Status</li>
	 * <li>Step 5: Navigate to the Gateway > Connection > Local IP Configuration
	 * page and verify navigation status</li>
	 * <li>Step 6: Navigate to the Gateway > Connection > Wi-Fi page and verify
	 * navigation status</li>
	 * <li>Step 7: Navigate to the Gateway > Connection > MoCA page and verify
	 * navigation status</li>
	 * <li>Step 8 : Navigate to the Gateway > Firewall > IPv4 page and verify
	 * navigation status</li>
	 * <li>Step 9 : Navigate to the Gateway > Firewall > IPv6 page and verify
	 * navigation status</li>
	 * <li>Step 10 : Navigate to the Gateway > Software page and verify navigation
	 * status</li>
	 * <li>Step 11 : Navigate to the Gateway > Hardware > System Hardware page</li>
	 * <li>Step 12 : Navigate to the Gateway > Hardware > LAN Ethernet page and
	 * verify navigation status</li>
	 * <li>Step 13 : Navigate to the Gateway > Hardware > Wireless page and verify
	 * navigation status</li>
	 * <li>Step 14 : Navigate to the Connected Devices > Devices page and verify
	 * navigation status</li>
	 * <li>Step 15 : Navigate to the Parental Control > Managed Sites page and
	 * verify navigation status</li>
	 * <li>Step 16 : Navigate to the Parental Control > Managed Services page and
	 * verify navigation status</li>
	 * <li>Step 17 : Navigate to the Parental Control > Managed Devices page and
	 * verify navigation status</li>
	 * <li>Step 18 : Navigate to the Parental Control > Reports page and verify
	 * navigation status</li>
	 * <li>Step 19 : Navigate to the Advanced > Port Forwarding page and verify
	 * navigation status</li>
	 * <li>Step 20 : Navigate to the Advanced > Port Triggering page and verify
	 * navigation status</li>
	 * <li>Step 21 : Navigate to the Advanced > Remote Management page and verify
	 * navigation status</li>
	 * <li>Step 22 : Navigate to the Advanced > DMZ page and verify navigation
	 * status</li>
	 * <li>Step 23 : Navigate to the Advanced > Device Discovery page and verify
	 * navigation status</li>
	 * <li>Step 24 : Navigate to the Troubleshooting > Logs page and verify
	 * navigation status</li>
	 * <li>Step 25 : Navigate to the Troubleshooting > Network Diagnostic Tools page
	 * and verify navigation status</li>
	 * <li>Step 26 : Navigate to the Troubleshooting > Wi-Fi Spectrum Analyzer page
	 * and verify navigation status</li>
	 * <li>Step 27 : Navigate to the Troubleshooting > MOCA Diagnostics page and
	 * verify navigation status</li>
	 * <li>Step 28 : Navigate to the Troubleshooting > Reset/Restore Gateway page
	 * and verify navigation status</li>
	 * <li>Step 29 : Navigate to the Troubleshooting > Change Password page and
	 * verify navigation status</li>
	 * <li>POST-CONDITION 1 : Close the Browser LAN Client</li>
	 * <li>POST-CONDITION 2 : Verify disconnecting the 5GHz private Wi-Fi SSID</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Vignesh
	 * @refactor Said Hisham
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WEB-GUI-ADMIN-6002")
	public void testToVerifyAllLanSidePageLaunchStatusInFirefoxBrowserUsing5GHz(Dut device) {
		Dut deviceConnectedWith5Ghz = null;
		String testCaseId = "TC-RDKB-WEB-GUI-ADMIN-602";
		int stepNumber = 1;
		stepNum = "S" + stepNumber;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WEB-GUI-ADMIN-6002");
		LOGGER.info(
				"TEST DESCRIPTION : Verify Admin GUI login and verify all pages are accessible in Firefox Browser for 2.4 GHz wifi");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 5 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify IPv4 is assigned on the 5GHz Wi-Fi client");
		LOGGER.info("PRE-CONDITION 3 : Verify IPv6 is assigned on the 5GHz Wi-Fi client");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected 5GHz Wi-Fi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi 5GHz Wi-Fi client using ipv6 interface");
		LOGGER.info(" 1: Verify login into the LAN GUI Adimn page by using valid userid and valid password");
		LOGGER.info(" 2: Navigate to the Gateway > At a Glance page and verify navigation status");
		LOGGER.info(" 3: Navigate to the Gateway > Connection > Status page and verify navigation status");
		LOGGER.info(
				" 4: Navigate to the Gateway>Connection>PARTNER Network page (Residential)  OR Gateway>Connection>Partner Network page(Commercial) and verify Navigation Status");
		LOGGER.info(
				" 5: Navigate to the Gateway > Connection > Local IP Configuration page and verify navigation status");
		LOGGER.info(" 6: Navigate to the Gateway > Connection > Wi-Fi page and verify navigation status");
		LOGGER.info(" 7: Navigate to the Gateway > Connection > MoCA page and verify navigation status");
		LOGGER.info(" 8 : Navigate to the Gateway > Firewall > IPv4 page and verify navigation status");
		LOGGER.info(" 9 : Navigate to the Gateway > Firewall > IPv6 page and verify navigation status");
		LOGGER.info(" 10 : Navigate to the Gateway > Software  page and verify navigation status");
		LOGGER.info(" 11 : Navigate to the Gateway > Hardware > System Hardware page");
		LOGGER.info(" 12 : Navigate to the Gateway > Hardware > LAN Ethernet page and verify navigation status");
		LOGGER.info(" 13 : Navigate to the Gateway > Hardware > Wireless page and verify navigation status");
		LOGGER.info(" 14 : Navigate to the Connected Devices > Devices page and verify navigation status");
		LOGGER.info(" 15 : Navigate to the Parental Control > Managed Sites page and verify navigation status");
		LOGGER.info(" 16 : Navigate to the Parental Control > Managed Services page and verify navigation status");
		LOGGER.info(" 17 : Navigate to the Parental Control > Managed Devices page and verify navigation status");
		LOGGER.info(" 18 : Navigate to the Parental Control > Reports page and verify navigation status");
		LOGGER.info(" 19 : Navigate to the Advanced > Port Forwarding page and verify navigation status");
		LOGGER.info(" 20 : Navigate to the Advanced > Port Triggering page and verify navigation status");
		LOGGER.info(" 21 : Navigate to the Advanced > Remote Management page and verify navigation status");
		LOGGER.info(" 22 : Navigate to the Advanced > DMZ page and verify navigation status");
		LOGGER.info(" 23 : Navigate to the Advanced > Device Discovery  page and verify navigation status");
		LOGGER.info(" 24 : Navigate to the Troubleshooting > Logs  page and verify navigation status");
		LOGGER.info(
				" 25 : Navigate to the Troubleshooting > Network Diagnostic Tools page and verify navigation status");
		LOGGER.info(
				" 26 : Navigate to the Troubleshooting > Wi-Fi Spectrum Analyzer page and  verify navigation status");
		LOGGER.info(" 27 : Navigate to the Troubleshooting >   Diagnostics page and  verify navigation status");
		LOGGER.info(" 28 : Navigate to the Troubleshooting > Reset/Restore Gateway  page and verify navigation status");
		LOGGER.info(" 29 : Navigate to the Troubleshooting > Change Password page and verify navigation status");
		LOGGER.info("POST-CONDITION 1 : Close the Browser LAN Client");
		LOGGER.info("POST-CONDITION 2 : Verify disconnecting the 5 GHz private wifi SSID");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnectedWith5Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
					tapEnv, BroadBandTestConstants.BAND_5GHZ);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			validateLanSidePageNavigationCrossBroserValidation(device, deviceConnectedWith5Ghz, testCaseId,
					Browser.FIREFOX);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-condition 1 : Close the LAN Side Browser
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			LOGGER.info("#######################################################################################");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			/**
			 * Post-condition 2 : Disconnect the 5 GHz private wifi SSID
			 */
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith5Ghz, BroadBandTestConstants.CONSTANT_2);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEB-GUI-ADMIN-6002");
	}

	/**
	 * Test method to validate the page navigation in Connected client
	 * 
	 * @param device          {@link Dut}
	 * @param deviceConnected instance of connected device
	 * @param testCaseId
	 * @param browser
	 * 
	 * @refactor Said hisham
	 */
	private static void validateLanSidePageNavigationCrossBroserValidation(Dut device, Dut deviceConnected,
			String testCaseId, Browser browser) {
		int stepNumber = 1;
		stepNum = "S" + stepNumber;
		WebDriver driver = null;
		LanSidePageNavigation lanSidePageNavigation = null;
		boolean isBusinessDevice = DeviceModeHandler.isBusinessClassDevice(device);
		boolean isFiberDevice = DeviceModeHandler.isFibreDevice(device);
		boolean isBusinessClassDevice = isBusinessDevice || isFiberDevice;

		/**
		 * Step 1 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING VALID USERID AND
		 * VALID PASSWORD
		 */
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : VERIFY THE GATEWAY ADMIN PAGE IS ACCESSIBLE IN CONNECTED CLIENT AND CAN BE LOGGED IN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : LAUNCH THE GATEWAY ADMIN GUI IN BROWSER URL : https://10.0.0.1 or https://10.1.10.1 , ONCE THE PAGE IS LOADED ,USE USERNAME AND PASSWORD AS ADMIN/****** FOR RESIDENTIAL OR CUSADMIN/****** FOR COMMERCIAL DEVICES TO LOGIN");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : GATEWAY ADMIN PAGE SHOULD BE ACCESSIBLE FROM CLIENT AND CAN BE ABLE TO LOGIN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO LOGIN GATEWAY ADMIN PAGE IN CONNECTED CLIENT";
		try {
			status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected, Browser.FIREFOX);
			isBrowserOpen = status;
			driver = LanWebGuiLoginPage.getDriver();
			LOGGER.info(" webDriver " + driver);
			lanSidePageNavigation = new LanSidePageNavigation(driver);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error("Exception occurred during Gateway Admin Page login : " + errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : LAN GUI ADMIN LOGIN SUCCESSFUL");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		/**
		 * Step 2 : NAVIGATE TO THE GATEWAY > AT A GLANCE PAGE AND VERIFY NAVIGATION
		 * STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > AT A GLANCE PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > AT A GLANCE");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > AT A GLANCE PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > AT A GLANCE PAGE";
		BroadBandAtGlancePage homepage = new BroadBandAtGlancePage(driver);
		status = homepage.verifyAtGlancePageLaunchedStatus();
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL AND IT DISPLAYED THE GATEWAY > AT A GLANCE PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 3 : NAVIGATE TO THE GATEWAY > CONNECTION > STATUS PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > STATUS PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > STATUS");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > STATUS PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > STATUS PAGE";
		status = lanSidePageNavigation.navigateToConnectionStatusPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL AND IT DISPLAYED THE GATEWAY > CONNECTION > STATUS PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		/**
		 * Step 4 : NAVIGATE TO THE GATEWAY > CONNECTION > PARTNER NETWORK PAGE FOR
		 * RESIDENTIAL OR GATEWAY > CONNECTION > PARTNER NETWORK PAGE FOR COMMERCIAL
		 * DEVICES AND VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > PARTNER NETWORK PAGE FOR RESIDENTIAL OR GATEWAY > CONNECTION > PARTNER NETWORK PAGE FOR COMMERCIAL DEVICES AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION : CLICK ON GATEWAY > CONNECTION > PARTNER NETWORK(FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PARTNER NETWORK(FOR COMMERCIAL DEVICE)");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > PARTNER NETWORK PAGE (FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PARTNER NETWORK PAGE (FOR COMMERCIAL DEVICE)");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > PARTNER NETWORK PAGE(FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PARTNER NETWORK(FOR COMMERCIAL DEVICE)PAGE";
		status = lanSidePageNavigation.navigateToPartnerNetworkPage(device, tapEnv, driver, isBusinessDevice);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > PARTNER NETWORK PAGE (FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PARTNER NETWORK PAGE (FOR COMMERCIAL DEVICE) ");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 5 : NAVIGATE TO THE GATEWAY > CONNECTION > LOCAL IP CONFIGURATION PAGE
		 * AND VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > LOCAL IP CONFIGURATION PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > LOCAL IP CONFIGURATION");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > LOCAL IP CONFIGURATION PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > LOCAL IP CONFIGURATION PAGE";
		status = lanSidePageNavigation.navigateToLocalIpConfigurationPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL :  NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > LOCAL IP CONFIGURATION PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 6 : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > WI-FI PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > WI-FI");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > WI-FI PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > WI-FI PAGE";
		status = lanSidePageNavigation.navigateToWiFiConfigurationPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > WI-FI PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 7 : NAVIGATE TO THE GATEWAY > CONNECTION > MOCA PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > MOCA PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > CONNECTION > MOCA");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > MOCA PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > MOCA PAGE";
		if (!isBusinessClassDevice) {
			status = lanSidePageNavigation.navigateToMoCAPage(device, tapEnv, driver);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > MOCA PAGE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);
		} else {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : PAGE NAVIGATION GATEWAY > CONNECTION > MOCA IS NOT APPLICABLE FOR COMMERCIAL DEVICES");
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					BroadBandTestConstants.NA_MSG_FOR_COMMERCIAL_DEVICES, false);
		}

		/**
		 * Step 8 : NAVIGATE TO THE GATEWAY > FIREWALL > IPV4 PAGE AND VERIFY NAVIGATION
		 * STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > FIREWALL > IPV4 PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > FIREWALL > IPV4");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > FIREWALL > IPV4 PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > FIREWALL > IPV4 PAGE";
		boolean result = LanSideBasePage.isPageLaunched(BroadBandWebGuiTestConstant.LINK_TEXT_FIREWALL,
				BroadbandPropertyFileHandler.getPageTitleForMoCA());
		if (result) {
			status = LanSideBasePage.isFireWallPageLaunchedForPartners(device, tapEnv,
					BroadBandWebGuiTestConstant.LINK_TEXT_IPV4, BroadBandTestConstants.FIREWALL_IPV4);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > FIREWALL > IPV4 PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		//
		/**
		 * Step 9 : NAVIGATE TO THE GATEWAY > FIREWALL > IPV6 PAGE AND VERIFY NAVIGATION
		 * STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > FIREWALL > IPV6 PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > FIREWALL > IPV6");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > FIREWALL > IPV6 PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > FIREWALL > IPV6 PAGE";
		status = LanSideBasePage.isFireWallPageLaunchedForPartners(device, tapEnv,
				BroadBandWebGuiTestConstant.LINK_TEXT_IPV6, BroadBandTestConstants.FIREWALL_IPV6);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > FIREWALL > IPV6 PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 10 : NAVIGATE TO THE GATEWAY > SOFTWARE PAGE AND VERIFY NAVIGATION
		 * STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > SOFTWARE PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > SOFTWARE");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > SOFTWARE PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > SOFTWARE PAGE";
		status = lanSidePageNavigation.navigateToSoftwarePage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > SOFTWARE PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 11 : NAVIGATE TO THE GATEWAY > HARDWARE > SYSTEM HARDWARE PAGE AND
		 * VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > HARDWARE > SYSTEM HARDWARE PAGE AND VERIFY NAVIGATION STATUS ");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > HARDWARE > SYSTEM HARDWARE");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > HARDWARE > SYSTEM HARDWARE PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > HARDWARE > SYSTEM HARDWARE PAGE";
		status = lanSidePageNavigation.navigateToHardwareFromSoftwarePage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > HARDWARE > SYSTEM HARDWARE PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 12 : NAVIGATE TO THE GATEWAY > HARDWARE > LAN ETHERNET PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > HARDWARE > LAN ETHERNET PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > HARDWARE > LAN ETHERNET");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > HARDWARE > LAN ETHERNET PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > HARDWARE > LAN ETHERNET PAGE";
		status = lanSidePageNavigation.navigateToHardwareLanPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > HARDWARE > LAN ETHERNET PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 13 : NAVIGATE TO THE GATEWAY > HARDWARE > WIRELESS PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > HARDWARE > WIRELESS PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON GATEWAY > HARDWARE > WIRELESS");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > HARDWARE > WIRELESS PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > HARDWARE > WIRELESS PAGE";
		status = lanSidePageNavigation.navigateToHardwareWirelessPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > HARDWARE > WIRELESS PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 14 : NAVIGATE TO THE CONNECTED DEVICES > DEVICES PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE CONNECTED DEVICES > DEVICES PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON CONNECTED DEVICES > DEVICES");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE CONNECTED DEVICES > DEVICES PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON CONNECTED DEVICES > DEVICES PAGE";
		status = lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR CONNECTED DEVICES > DEVICES PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 15 : NAVIGATE TO THE PARENTAL CONTROL > MANAGED SITES PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE PARENTAL CONTROL > MANAGED SITES PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON PARENTAL CONTROL > MANAGED SITES");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE PARENTAL CONTROL > MANAGED SITES PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON PARENTAL CONTROL > MANAGED SITES PAGE";
		status = lanSidePageNavigation.navigateToManagedSites(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR PARENTAL CONTROL > MANAGED SITES PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 16 : NAVIGATE TO THE PARENTAL CONTROL > MANAGED SERVICES PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE PARENTAL CONTROL > MANAGED SERVICES PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON PARENTAL CONTROL > MANAGED SERVICES");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE PARENTAL CONTROL > MANAGED SERVICES PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON PARENTAL CONTROL > MANAGED SERVICES PAGE";
		status = lanSidePageNavigation.navigateToManagedServices(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR PARENTAL CONTROL > MANAGED SERVICES PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 17 : NAVIGATE TO THE PARENTAL CONTROL > MANAGED DEVICES PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE PARENTAL CONTROL > MANAGED DEVICES PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON PARENTAL CONTROL > MANAGED DEVICES");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE PARENTAL CONTROL > MANAGED DEVICES PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON PARENTAL CONTROL > MANAGED DEVICES PAGE";
		status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlManagedDevices(device, tapEnv,
				BroadBandWebGuiTestConstant.LINK_TEXT_MANAGED_DEVICES, BroadBandTestConstants.MANAGED_DEVICES);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR PARENTAL CONTROL > MANAGED DEVICES PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 18 : NAVIGATE TO THE PARENTAL CONTROL > REPORTS PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE PARENTAL CONTROL > REPORTS PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON PARENTAL CONTROL > REPORTS");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE PARENTAL CONTROL > REPORTS PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON PARENTAL CONTROL > REPORTS PAGE";
		status = lanSidePageNavigation.navigateToReports(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR PARENTAL CONTROL > REPORTS PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 19 : NAVIGATE TO THE ADVANCED > PORT FORWARDING PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE ADVANCED > PORT FORWARDING PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON ADVANCED > PORT FORWARDING");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE ADVANCED > PORT FORWARDING PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON ADVANCED > PORT FORWARDING PAGE";
		status = lanSidePageNavigation.navigateToAdvancedPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR ADVANCED > PORT FORWARDING PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 20 : NAVIGATE TO THE ADVANCED > PORT TRIGGERING PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE ADVANCED > PORT TRIGGERING PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON ADVANCED > PORT TRIGGERING");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE ADVANCED > PORT TRIGGERING PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON ADVANCED > PORT TRIGGERING PAGE";
		status = lanSidePageNavigation.navigateToAdvancedTriggerPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR ADVANCED > PORT TRIGGERING PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 21 : NAVIGATE TO THE ADVANCED > REMOTE MANAGEMENT PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE ADVANCED > REMOTE MANAGEMENT PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON ADVANCED > REMOTE MANAGEMENT");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE ADVANCED > REMOTE MANAGEMENT PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON ADVANCED > REMOTE MANAGEMENT PAGE";
		status = lanSidePageNavigation.navigateToAdvancedRemoteMgmtPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR ADVANCED > REMOTE MANAGEMENT PAGE PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		//
		/**
		 * Step 22 : NAVIGATE TO THE ADVANCED > DMZ PAGE AND VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE ADVANCED > DMZ PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON ADVANCED > DMZ");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE ADVANCED > DMZ PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON ADVANCED > DMZ PAGE";
		status = lanSidePageNavigation.navigateToAdvancedDmzPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR ADVANCED > DMZ PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 23 : NAVIGATE TO THE ADVANCED > DEVICE DISCOVERY PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE ADVANCED > DEVICE DISCOVERY PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON ADVANCED > DEVICE DISCOVERY");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE ADVANCED > DEVICE DISCOVERY PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON ADVANCED > DEVICE DISCOVERY PAGE";
		status = lanSidePageNavigation.navigateToAdvancedDeviceDiscoverPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR ADVANCED > DEVICE DISCOVERY PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
		//
		/**
		 * Step 24 : NAVIGATE TO THE TROUBLESHOOTING > LOGS PAGE AND VERIFY NAVIGATION
		 * STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE TROUBLESHOOTING > LOGS PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON TROUBLESHOOTING > LOGS");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE TROUBLESHOOTING > LOGS PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON TROUBLESHOOTING > LOGS PAGE";
		status = lanSidePageNavigation.navigateToTroubleShootingLogsPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR TROUBLESHOOTING > LOGS PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 25 : NAVIGATE TO THE TROUBLESHOOTING > NETWORK DIAGNOSTIC TOOLS PAGE AND
		 * VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE TROUBLESHOOTING > NETWORK DIAGNOSTIC TOOLS PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON TROUBLESHOOTING > NETWORK DIAGNOSTIC TOOLS");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE TROUBLESHOOTING > NETWORK DIAGNOSTIC TOOLS PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON TROUBLESHOOTING > NETWORK DIAGNOSTIC TOOLS PAGE";
		status = lanSidePageNavigation.navigateToTroubleShootingNwDiagToolsPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR TROUBLESHOOTING > NETWORK DIAGNOSTIC TOOLS PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 26 : NAVIGATE TO THE TROUBLESHOOTING > WI-FI SPECTRUM ANALYZER PAGE AND
		 * VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE TROUBLESHOOTING > WI-FI SPECTRUM ANALYZER PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON TROUBLESHOOTING > WI-FI SPECTRUM ANALYZER");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE TROUBLESHOOTING > WI-FI SPECTRUM ANALYZER PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON TROUBLESHOOTING > WI-FI SPECTRUM ANALYZER PAGE";
		if (!isBusinessClassDevice) {
			status = lanSidePageNavigation.navigateToTroubleShootingWifiSpectrumAnalyzerPage(device, tapEnv, driver);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR TROUBLESHOOTING > WI-FI SPECTRUM ANALYZER PAGE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);
		} else {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : PAGE NAVIGATION TROUBLESHOOTING > WI-FI SPECTRUM ANALYZER IS NOT APPLICABLE FOR COMMERCIAL DEVICES");
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					BroadBandTestConstants.NA_MSG_FOR_COMMERCIAL_DEVICES, false);
		}

		/**
		 * Step 27 : NAVIGATE TO THE TROUBLESHOOTING > MOCA DIAGNOSTICS PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE TROUBLESHOOTING > MOCA DIAGNOSTICS PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON TROUBLESHOOTING > MOCA DIAGNOSTICS");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE TROUBLESHOOTING > MOCA DIAGNOSTICS PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON TROUBLESHOOTING > MOCA DIAGNOSTICS PAGE";
		if (!isBusinessClassDevice) {
			status = lanSidePageNavigation.navigateToTroubleShootingMoCADiagnosticsPage(device, tapEnv, driver);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR TROUBLESHOOTING > MOCA DIAGNOSTICS PAGE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);
		} else {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : PAGE NAVIGATION TROUBLESHOOTING > MOCA DIAGNOSTICS IS NOT APPLICABLE FOR COMMERCIAL DEVICES");
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					BroadBandTestConstants.NA_MSG_FOR_COMMERCIAL_DEVICES, false);
		}

		/**
		 * Step 28 : NAVIGATE TO THE TROUBLESHOOTING > RESET / RESTORE GATEWAY PAGE AND
		 * VERIFY NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE TROUBLESHOOTING > RESET / RESTORE GATEWAY PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON TROUBLESHOOTING > RESET / RESTORE GATEWAY");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE TROUBLESHOOTING > RESET / RESTORE GATEWAY PAGE");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON TROUBLESHOOTING > RESET / RESTORE GATEWAY PAGE";
		status = lanSidePageNavigation.navigateToTroubleShootingResetRestorePage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR TROUBLESHOOTING > RESET / RESTORE GATEWAY PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);

		/**
		 * Step 29 : NAVIGATE TO THE TROUBLESHOOTING > CHANGE PASSWORD PAGE AND VERIFY
		 * NAVIGATION STATUS
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : NAVIGATE TO THE TROUBLESHOOTING > CHANGE PASSWORD PAGE AND VERIFY NAVIGATION STATUS");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON TROUBLESHOOTING > CHANGE PASSWORD");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE TROUBLESHOOTING > CHANGE PASSWORD page");
		LOGGER.info("**********************************************************************************");
		errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON TROUBLESHOOTING > CHANGE PASSWORD PAGE";
		status = lanSidePageNavigation.navigateToTroubleShootingChangePwdPage(device, tapEnv, driver);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR TROUBLESHOOTING > CHANGE PASSWORD PAGE");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, false);
	}

	/**
	 *
	 * Test Case : Verify access to DUT via UserAdmin page - WAN IP using Windows OS
	 * and Internet Explorer browser (HTTPS Disabled and Enabled)
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify the private wifi 2.4 GHz and 5 GHz ssid's are
	 * broadcasting in connected client.</li>
	 * <li>Step 1 : Connect the client to 2.4/5 GHz Private Wi-Fi Network and verify
	 * connection status.</li>
	 * <li>Step 2 : Verify the correct IPv4 address for client connected with 2.4/5
	 * GHz SSID.</li>
	 * <li>Step 3 : Verify the correct IPv6 address for client connected with 2.4/5
	 * GHz SSID.</li>
	 * <li>Step 4 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4/5 GHz.</li>
	 * <li>Step 5 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4/5 GHz.</li>
	 * <li>Step 6 : Verify login into the LAN GUI Adimn page by using valid userid
	 * and valid password using Internet Explorer Browser.</li>
	 * <li>Step 7 : Navigate to the Advanced > Port Forwarding page and verify
	 * navigation status.</li>
	 * <li>Step 8 : Verify Port Forwarding is in Disabled State.</li>
	 * <li>Step 9 : Navigate to the Advanced > Remote Management page and verify
	 * navigation status.</li>
	 * <li>Step 10 : Verify Enable Button for HTTPS 8181 is in disabled state.</li>
	 * <li>Step 11 : Verify WEBPA command to get the WANIPV6 of the Client is
	 * Successful.</li>
	 * <li>Step 12 : Verify disconnecting the client connected with 2.4/5 GHz SSID
	 * and 5GHz SSID in the setup</li>
	 * <li>POST-CONDITION 1 : Revert the HTTPS 8181 in Remote Management to its
	 * default value. .</li>
	 * 
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Elangovan
	 * @refactor Govardhan
	 *
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WEB-GUI-IE-5001")
	public void VerifyAccessByWanIpAfterHttpsEnabledInIe2GHz(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WEB-GUI-IE-501";
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		List<Dut> ssidVisibleDevices = new ArrayList<Dut>();
		Dut ssidVisibleDevice = null;
		int preConStepNumber = 1;
		// Variable Declaration Ends
		try {
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WEB-GUI-IE-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify access to DUT via UserAdmin page - WAN IP using Windows OS and Internet Explorer browser (HTTPS Disabled and Enabled).");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					" PRE CONDITION 1 : Verify the private wifi 2.4 GHz and 5 GHz ssid's are broadcasting in connected client.");
			LOGGER.info("Step 1. Connect the client to 2.4/5 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info("Step 2. Verify  the correct IPv4  address for client connected with 2.4/5 GHz SSID");
			LOGGER.info("Step 3. Verify  the correct IPv6  address for client connected with 2.4/5 GHz SSID");
			LOGGER.info(
					"Step 4. Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4/5 GHz ");
			LOGGER.info(
					"Step 5. Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4/5 GHz ");
			LOGGER.info(
					"Step 6. Verify login into the LAN GUI Adimn page by using valid userid and valid password using Internet Explorer browser");
			LOGGER.info("Step 7. Navigate to the Advanced > Port Forwarding page and verify navigation status");
			LOGGER.info("Step 8. Verify Port Forwarding is in Disabled State.");
			LOGGER.info("Step 9. Navigate to the Advanced > Remote Management page and verify navigation status");
			LOGGER.info("Step 10. Verify Enable Button for HTTPS 8181 is in disabled state.");
			LOGGER.info("Step 11. Verify WEBPA command to get the WANIPV6 of the Client is Successful. ");
			LOGGER.info(
					"Step 12. Verify disconnecting the client connected with 2.4/5 GHz SSID and GHz SSID in the setup");
			LOGGER.info(" POST CONDITION 1 : Revert the HTTPS 8181 in Remote Management to its default value.");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			/**
			 * PRE-CONDITION 1-2 : VERIFY THE PRIVATE WIFI 2.4 GHZ AND 5 GHZ SSID'S ARE
			 * ENABLED
			 */
			BroadBandPreConditionUtils.executePreConditionToVerifyRadioStatus(device, tapEnv, preConStepNumber);
			/**
			 * PRE-CONDITION 3 : VERIFY THE PRIVATE WIFI 2.4 GHZ AND 5 GHZ SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber = 3;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_1);
			ssidVisibleDevice = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			BroadBandWebUiBaseTest broadBandWebUiBaseTest = new BroadBandWebUiBaseTest();
			broadBandWebUiBaseTest.invokeBrowser();
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			/**
			 * Step 1-15 : VERIFY ACCESS TO DUT VIA USERADMIN PAGE - WAN (OR) EROUTER IP
			 * USING WINDOWS OS AND INTERNET EXPLORER BROWSER.
			 */
			executeTestStepToVerifyAccessToDut(device, ssidVisibleDevice, testCaseId,
					BroadBandTestConstants.BAND_2_4GHZ_OR_5GHZ, Browser.IE, driver);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING ACCESS TO DUT VIA USERADMIN PAGE - WAN (OR) EROUTER IP USING WINDOWS OS AND INTERNET EXPLORER BROWSER : "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEB-GUI-IE-5001");
		LOGGER.info("**********************************************************************************");
	}

	/**
	 * 
	 * Test Case : Verify access to DUT via UserAdmin page - WAN IP using Windows OS
	 * and Firefox browser (HTTPS Disabled and Enabled)
	 * 
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify the private wifi 2.4 GHz ssid's are broadcasting
	 * in connected client.</li>
	 * <li>Connect the client to 2.4/5 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>Verify the correct IPv4 address for client connected with 2.4 GHz
	 * SSID</li>
	 * <li>Verify the correct IPv6 address for client connected with 2.4 GHz
	 * SSID</li>
	 * <li>Verify whether have connectivity using that particular interface using
	 * IPV4 for client connected with 2.4 GHz</li>
	 * <li>Verify whether have connectivity using that particular interface using
	 * IPV6 for client connected with 2.4 GHz</li>
	 * <li>Verify login into the LAN GUI Adimn page by using valid userid and valid
	 * password</li>
	 * <li>Navigate to the Advanced > Port Forwarding page and verify navigation
	 * status</li>
	 * <li>Verify Port Forwarding is in Disabled State.</li>
	 * <li>Navigate to the Advanced > Remote Management page and verify navigation
	 * status</li>
	 * <li>Verify Enable Button for HTTPS 8181 is in disabled state.</li>
	 * <li>Verify WEBPA command to get the WANIPV6 of the Client is Successful.</li>
	 * <li>Verify disconnecting the client connected with 2.4 GHz SSID and 5Ghz SSID
	 * in the setup</li>
	 * <li>POST-CONDITION : Revert the HTTPS 8181 in Remote Management to its
	 * default value.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Manikandan T
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-WEB-GUI-FF-5004")
	public void testToVerifyRemoteManagementFor2GHz(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WEB-GUI-FF-504";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		int preConStepNumber = 1;
		List<Dut> ssidVisibleDevices = null;
		Dut ssidVisibleDevice2GHz = null;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WEB-GUI-FF-5004");
		LOGGER.info(
				"TEST DESCRIPTION: Verify access to DUT via UserAdmin page - WAN IP using Windows OS and Firefox browser (HTTPS Disabled and Enabled)");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				" PRE CONDITION 1 : Verify the private wifi 2.4 GHz and 5 GHz ssid's are broadcasting in connected client.");
		LOGGER.info("1. Connect the client to 2.4 GHz Private Wi-Fi Network and verify connection status ");
		LOGGER.info("2. Verify  the correct IPv4  address for client connected with 2.4 GHz SSID");
		LOGGER.info("3. Verify  the correct IPv6  address for client connected with 2.4 GHz SSID");
		LOGGER.info(
				"4. Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
		LOGGER.info(
				"5. Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
		LOGGER.info("6. Verify login into the LAN GUI Adimn page by using valid userid and valid password");
		LOGGER.info("7. Navigate to the Advanced > Port Forwarding page and verify navigation status");
		LOGGER.info("8. Verify Port Forwarding is in Disabled State.");
		LOGGER.info("9. Navigate to the Advanced > Remote Management page and verify navigation status");
		LOGGER.info("10. Verify Enable Button for HTTPS 8181 is in disabled state.");
		LOGGER.info("11. Verify WEBPA command to get the WANIPV6 of the Client is Successful. ");
		LOGGER.info("12. Verify disconnecting the client connected with 2.4 GHz SSID and 5Ghz SSID in the setup");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : VERIFY THE PRIVATE WIFI 2.4 GHZ AND 5 GHZ SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber = 1;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_1);
			ssidVisibleDevice2GHz = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			BroadBandWebUiBaseTest broadBandWebUiBaseTest = new BroadBandWebUiBaseTest();
			broadBandWebUiBaseTest.invokeBrowser();
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			/**
			 * Step 1-15 : VERIFY ACCESS TO DUT VIA USERADMIN PAGE - WAN (OR) EROUTER IP
			 * USING WINDOWS OS AND FIREFOX BROWSER.
			 */
			executeTestStepToVerifyAccessToDut(device, ssidVisibleDevice2GHz, testCaseId,
					BroadBandTestConstants.BAND_2_4GHZ, Browser.FIREFOX, driver);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEB-GUI-FF-5004");
	}

	/**
	 * 
	 * Test Case : Verify access to DUT via UserAdmin page - WAN IP using Windows OS
	 * and Firefox browser (HTTPS Disabled and Enabled)
	 * 
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify the private wifi 5 GHz ssid's are broadcasting
	 * in connected client.</li>
	 * <li>Connect the client to 5 GHz Private Wi-Fi Network and verify connection
	 * status</li>
	 * <li>Verify the correct IPv4 address for client connected with 5 GHz SSID</li>
	 * <li>Verify the correct IPv6 address for client connected with 5 GHz SSID</li>
	 * <li>Verify whether have connectivity using that particular interface using
	 * IPV4 for client connected with 5 GHz</li>
	 * <li>Verify whether have connectivity using that particular interface using
	 * IPV6 for client connected with 5 GHz</li>
	 * <li>Verify login into the LAN GUI Admin page by using valid userid and valid
	 * password</li>
	 * <li>Navigate to the Advanced > Port Forwarding page and verify navigation
	 * status</li>
	 * <li>Verify Port Forwarding is in Disabled State.</li>
	 * <li>Navigate to the Advanced > Remote Management page and verify navigation
	 * status</li>
	 * <li>Verify Enable Button for HTTPS 8181 is in disabled state.</li>
	 * <li>Verify WEBPA command to get the WANIPV6 of the Client is Successful.</li>
	 * <li>Verify disconnecting the client connected with 5 GHz SSID and 5Ghz SSID
	 * in the setup</li>
	 * <li>POST-CONDITION : Revert the HTTPS 8181 in Remote Management to its
	 * default value.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Manikandan T
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-WEB-GUI-FF-5005")
	public void testToVerifyRemoteManagementFor5GHz(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WEB-GUI-FF-505";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		int preConStepNumber = 1;
		List<Dut> ssidVisibleDevices = null;
		Dut ssidVisibleDevice5GHz = null;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WEB-GUI-FF-5005");
		LOGGER.info(
				"TEST DESCRIPTION: Verify access to DUT via UserAdmin page - WAN (or) eRouter IP using Windows OS and Firefox browser (HTTPS Disabled and Enabled)");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(" PRE CONDITION 1 : Verify the private wifi 5 GHz ssid's are broadcasting in connected client.");
		LOGGER.info("1. Connect the client to 5 GHz Private Wi-Fi Network and verify connection status ");
		LOGGER.info("2. Verify  the correct IPv4  address for client connected with 5 GHz SSID");
		LOGGER.info("3. Verify  the correct IPv6  address for client connected with 5 GHz SSID");
		LOGGER.info(
				"4. Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
		LOGGER.info(
				"5. Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
		LOGGER.info("6. Verify login into the LAN GUI Adimn page by using valid userid and valid password");
		LOGGER.info("7. Navigate to the Advanced > Port Forwarding page and verify navigation status");
		LOGGER.info("8. Verify Port Forwarding is in Disabled State.");
		LOGGER.info("9. Navigate to the Advanced > Remote Management page and verify navigation status");
		LOGGER.info("10. Verify Enable Button for HTTPS 8181 is in disabled state.");
		LOGGER.info("11. Verify WEBPA command to get the WANIPV6 of the Client is Successful. ");
		LOGGER.info("12. Verify disconnecting the client connected with 2.4/5 GHz SSID and 5Ghz SSID in the setup");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : VERIFY THE PRIVATE WIFI 5 GHZ SSID'S ARE BROADCASTING IN
			 * CONNECTED CLIENT
			 */
			preConStepNumber = 1;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_1);
			ssidVisibleDevice5GHz = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			BroadBandWebUiBaseTest broadBandWebUiBaseTest = new BroadBandWebUiBaseTest();
			broadBandWebUiBaseTest.invokeBrowser();
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			/**
			 * Step 1-15 : VERIFY ACCESS TO DUT VIA USERADMIN PAGE - WAN (OR) EROUTER IP
			 * USING WINDOWS OS AND FIREFOX BROWSER.
			 */
			executeTestStepToVerifyAccessToDut(device, ssidVisibleDevice5GHz, testCaseId,
					BroadBandTestConstants.BAND_5GHZ, Browser.FIREFOX, driver);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEB-GUI-FF-5005");
	}

	/**
	 * Method to Verify access to DUT via UserAdmin page - WAN IP using Windows OS
	 * 
	 * @param device            {@link Dut}
	 * @param ssidVisibleDevice Private wifi visible device
	 * @param testId            Test case ID
	 * @param wifiBand          Step Number
	 * @param wifiBand          Frequency band 2.4/5 GHz
	 * @param name              Instance of{@link Browser}
	 * @param driver            Instance of{@link WebDriver}
	 * @refactor Govardhan
	 */
	private static void executeTestStepToVerifyAccessToDut(Dut device, Dut ssidVisibleDevice, String testCaseId,
			String wifiBand, Browser browser, WebDriver driver) {
		boolean status = false;
		String errorMessage = null;
		WebDriver lanDriver = null;
		int stepNumber = 1;
		int postConStepNumber = 1;
		LanSidePageNavigation lanSidePageNavigation = null;
		try {
			/**
			 * STEP 1 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4/5 GHZ SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDevice, wifiBand,
					stepNumber);

			/**
			 * STEP 2-5 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED WITH
			 * 2.4GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDevice, stepNumber);

			/**
			 * STEP 6 : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING USERID AND
			 * PASSWORD
			 */
			stepNumber = 6;
			status = false;
			errorMessage = null;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : VERIFY THE GATEWAY ADMIN PAGE IS ACCESSIBLE IN CONNECTED CLIENT AND CAN BE LOGGED IN USING CUSADMIN/****** CREDENTIAL");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : LAUNCH THE GATEWAY ADMIN GUI IN BROWSER URL :https://10.1.10.1 , ONCE THE PAGE IS LOADED ,USE USERNAME AND PASSWORD AS CUSADMIN/****** TO LOGIN");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : GATEWAY ADMIN PAGE SHOULD BE ACCESSIBLE FROM CLIENT AND CAN BE ABLE TO LOGIN USING CUSADMIN/****** CREDENTIAL ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO LOGIN GATEWAY ADMIN PAGE IN CONNECTED CLIENT";
			try {
				status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, ssidVisibleDevice, browser);
				isBrowserOpen = status;
				lanDriver = LanWebGuiLoginPage.getDriver();
				LOGGER.info(" LAn webDriver " + lanDriver);
				lanSidePageNavigation = new LanSidePageNavigation(lanDriver);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error("EXCEPTION OCCURRED DURING GATEWAY ADMIN PAGE LOGIN : " + errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : LAN GUI ADMIN LOGIN SUCCESSFUL");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(lanDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			/**
			 * Step 7 : NAVIGATE TO THE ADVANCED > PORT FORWARDING PAGE AND VERIFY
			 * NAVIGATION STATUS
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : NAVIGATE TO THE ADVANCED > PORT FORWARDING PAGE AND VERIFY NAVIGATION STATUS");
			LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON ADVANCED > PORT FORWARDING");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE ADVANCED > PORT FORWARDING PAGE");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON ADVANCED > PORT FORWARDING PAGE";
			status = lanSidePageNavigation.navigateToAdvancedPage(device, tapEnv, lanDriver);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : NAVIGATION SUCCESSFUL FOR ADVANCED > PORT FORWARDING PAGE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(lanDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			/**
			 * Step 8 : VERIFY PORT FORWARDING IS IN DISABLED STATE.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY PORT FORWARDING IS IN DISABLED STATE.");
			LOGGER.info("STEP " + stepNumber + " : ACTION : CHECK THE PORT FORWARDING BUTTON IS DISABLED.");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : PORT FORWARDING SHOULD BE IN  DISABLED STATE.");
			LOGGER.info("**********************************************************************************");
			status = !BroadBandCommonPage.getPortForwardingModeEnabledStatus(lanDriver, device);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Port forwarding mode is in disabled state");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(lanDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, false);

			/**
			 * Step 9 : NAVIGATE TO THE ADVANCED > REMOTE MANAGEMENT PAGE AND VERIFY
			 * NAVIGATION STATUS
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : NAVIGATE TO THE ADVANCED > REMOTE MANAGEMENT PAGE AND VERIFY NAVIGATION STATUS");
			LOGGER.info("STEP " + stepNumber + " : ACTION : CLICK ON ADVANCED > REMOTE MANAGEMENT");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE ADVANCED > REMOTE MANAGEMENT PAGE");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON ADVANCED > REMOTE MANAGEMENT PAGE";
			status = lanSidePageNavigation.navigateToAdvancedRemoteMgmtPage(device, tapEnv, lanDriver);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NAVIGATION SUCCESSFUL FOR ADVANCED > REMOTE MANAGEMENT PAGE PAGE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(lanDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			/**
			 * Step 10 : VERIFY ENABLE BUTTON FOR HTTPS 8181 IS IN DISABLED STATE.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY ENABLE BUTTON FOR HTTPS 8181 IS IN DISABLED STATE.");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION :  CHECK THE ENABLE BUTTON FOR HTTPS 8181.");
			LOGGER.info(
					"STEP :  " + stepNumber + " : EXPECTED: ENABLE BUTTON FOR HTTPS 8181 SHOULD BE IN DISABLED STATE.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO VERIFY ENABLE BUTTON FOR HTTPS 8181 IS IN DISABLED STATE.";
			status = !BroadBandCommonPage.getHttpsModeEnabledStatus(lanDriver, device);
			// if https port 8181 is enabled,disabling through webpa.
			if (!status) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTPS_REMOTE_ACCESSSTATUS,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
			}
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL : HTTPS 8181 IS IN DISABLED STATE.");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(lanDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			/**
			 * Step 11 : VERIFY WEBPA COMMAND TO GET THE WANIPV6 OF THE CLIENT IS
			 * SUCCESSFUL.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY WEBPA COMMAND TO GET THE WANIPV6 OF THE CLIENT IS SUCCESSFUL.");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION :  Device.DeviceInfo.X_COMCAST-COM_WAN_IPv6");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: WEBPA GET SHOULD BE SUCCESSFUL AND WANIPV6 SHOULD BE RETRIEVED SUCCESFULLY.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO GET THE WAN IPV6 USING THE WEBPA PARAMETER.";
			String wanIpv6Address = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6);
			LOGGER.info("Wan Ipv6 Address = " + wanIpv6Address);
			status = CommonMethods.isNotNull(wanIpv6Address);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL : WANIPV6 ADDRESS IS RETRIEVED SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 12 : VERIFY DISCONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4/5 GHZ
			 * SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDevice, wifiBand,
					stepNumber);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING ACCESS TO DUT VIA USERADMIN PAGE - WAN (OR) EROUTER IP : "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			/**
			 * POST-CONDITION 1 : DISABLE THE HTTPS 8181 IN REMOTE MANAGEMENT THROUGH WEBPA
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : DESCRIPTION : REVERT THE HTTPS  PORT 8181 IN REMOTE MANAGEMENT TO ITS DEFAULT VALUE AS FALSE.");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : ACTION : EXECUTE WEBPA COMMAND : Device.UserInterface.X_CISCO_COM_RemoteAccess.HttpsEnable");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : EXPECTED : REVERTING THE HTTPS PORT 8181 IN REMOTE MANAGEMENT TO ITS DEFAULT VALUE SHOULD BE SUCCESSFUL.");
			LOGGER.info("#######################################################################################");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTPS_REMOTE_ACCESSSTATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : REVERTING THE HTTPS 8181 IN REMOTE MANAGEMENT TO ITS DEFAULT VALUE AS FALSE IS SUCCESSFUL");
			} else {
				LOGGER.error("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : REVERTING THE HTTP 8181 IN REMOTE MANAGEMENT TO ITS DEFAULT VALUE AS FALSE FAILED");
			}
			/**
			 * POST-CONDITION 2 : CLOSE THE LAN SIDE BROWSER
			 */
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
			LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
			if (isBrowserOpen) {
				try {
					LanSidePageNavigation.closeBrowser();
					LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
				} catch (Exception exception) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
				}
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
	}
}
