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

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.wifi.BroadBandWifiBaseTest;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiElements;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiTestConstant;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
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
import com.automatics.rdkb.webui.page.LanSideBasePage;
import com.automatics.rdkb.webui.page.LanSidePageNavigation;
import com.automatics.rdkb.webui.page.LanSideWizardPage;

public class BroadBandWebGuiAdminPageTest extends BroadBandWifiBaseTest {

    /** Constant holds the browser status flag **/
    private static boolean isBrowserOpen = false;

    /**
     *
     * Test Case : Verify the password validations when client connected to 2.4GHz
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify connection status</li>
     * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID</li>
     * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID</li>
     * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface</li>
     * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface</li>
     * <li>Step 1 : Launch Broad band User Admin page login page and verify login status</li>
     * <li>Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and verify navigation status</li>
     * <li>Step 3 : Verify the home network password cannot be set as blank</li>
     * <li>Step 4 : Verify the home network password cannot be set with space in password</li>
     * <li>Step 5 : Verify the home network password cannot be set with special character in password</li>
     * <li>Step 6 : Verify the home network password cannot be set with password mismatch</li>
     * <li>POST-CONDITION 1 : Verify disconnecting the 2.4 GHz private wifi SSID</li>
     * <li>POST-CONDITION 2 : Close the Browser LAN Client</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
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
     * <li>PRE-CONDITION 1 : Connect the client setup to 5 GHz SSID and verify connection status</li>
     * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with 5 GHz SSID</li>
     * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with 5 GHz SSID</li>
     * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface</li>
     * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface</li>
     * <li>Step 1 : Launch Broad band User Admin page login page and verify login status</li>
     * <li>Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and verify navigation status</li>
     * <li>Step 3 : Verify the home network password cannot be set as blank</li>
     * <li>Step 4 : Verify the home network password cannot be set with space in password</li>
     * <li>Step 5 : Verify the home network password cannot be set with special character in password</li>
     * <li>Step 6 : Verify the home network password cannot be set with password mismatch</li>
     * <li>POST-CONDITION 1 : Verify disconnecting the 5 GHz private wifi SSID</li>
     * <li>POST-CONDITION 2 : Close the Browser LAN Client</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
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
     * <li>PRE-CONDITION 1 : Connect the client setup to Ethernet and verify connection status</li>
     * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with Ethernet</li>
     * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with Ethernet</li>
     * <li>PRE-CONDITION 4 : Verify internet is accessible by using Interface IPv4 on the ethernet client</li>
     * <li>PRE-CONDITION 5 : Verify internet is accessible by using Interface IPv6 on the ethernet client</li>
     * <li>Step 1 : Launch Broad band User Admin page login page and verify login status</li>
     * <li>Step 2 : Navigate to the Gateway > Home Network Wizard - Step 1 page and verify navigation status</li>
     * <li>Step 3 : Verify the home network password cannot be set as blank</li>
     * <li>Step 4 : Verify the home network password cannot be set with space in password</li>
     * <li>Step 5 : Verify the home network password cannot be set with special character in password</li>
     * <li>Step 6 : Verify the home network password cannot be set with password mismatch</li>
     * <li>POST-CONDITION 1 : Close the Browser LAN Client</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
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
     * @param device
     *            {@link Dut}
     * @param tapEnv
     *            instance of {@link AutomaticsTapApi}
     * @param deviceConnected
     *            instance of connected device
     * @param testCaseId
     *            Test case ID
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
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Naviagte to Admin login page using admin/password credentials");
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
	 * Step 2 : NAVIGATE TO THE GATEWAY > HOME NETWORK WIZARD - STEP 1 PAGE AND VERIFY NAVIGATION STATUS
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
	 * Step 4 : VERIFY THE HOME NETWORK PASSWORD CANNOT BE SET WITH SPACE IN PASSWORD
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
	 * Step 5 : VERIFY THE HOME NETWORK PASSWORD CANNOT BE SET WITH WITH SPECIAL CHARACTER IN PASSWORD
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
	 * Step 6 : VERIFY THE HOME NETWORK PASSWORD CANNOT BE SET WITH PASSWORD MISMATCH
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
			LOGGER.info("POST-CONDITION 3 : Enable CloudUi and Mesh to remove xFi in LAN Admin GUI");
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
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv6 Address' or LINUX : ifconfig | grep 'inet6 ' ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO GET THE CORRECT IPV6 ADDRESS FROM CLIENT";
			status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					deviceConnected, tapEnv);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV6 ADDRESS FROM CLIENT");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

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
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED USING IPV6");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v 'www.google.com' | grep '200 OK' OR ping -6 -n 5 google.com , LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -6 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4 ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT WITH USING IPV6";
			broadBandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
					tapEnv, deviceConnected,
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
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY USING IPV6");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

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
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Launch the IPv6 FireWall page from partner page");
			LOGGER.info("STEP " + stepNumber + " : ACTION : Navigate to IPv6 Firewall page by clicking on \"IPv6\"");
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
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			/**
			 * Step 11 : Verify the Default IPv6 firewall mode is set to Typical Security
			 * (default)
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Default firewall mode for IPv6 retrieved from Web GUI is not 'Typical Security (default)'";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify the Default IPv6 firewall mode is set to Typical Security (default)");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Retrieve the firewall level selected in \"Gateway > Firewall> IPv6\" Page");
			LOGGER.info(
					"STEP " + stepNumber + " : EXPECTED : Default firewall mode should be Typical Security (default)");
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
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

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

}
